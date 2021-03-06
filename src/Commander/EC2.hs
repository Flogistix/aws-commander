{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs        #-}

#include "../macros.h"

module Commander.EC2 where

import Data.List
import Data.Monoid

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Lens.Prism
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State

import Control.Monad.Trans.Resource

import Data.Text (Text)
import qualified Data.Text.Encoding  as Text
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text

import System.IO

import Network.AWS
import Network.AWS.EC2
import Network.AWS.Data.Text

import Katip

import Commander.Conf
import Commander.Types
import Commander.EC2.SecurityGroup
import Commander.Utils

import qualified Data.ByteString.Base64 as B64

import Debug.Trace
  

userDataScript :: Int -> Text -> Text -> Text -> Text
userDataScript port region bucket s3Key = Text.decodeUtf8 . B64.encode . Text.encodeUtf8 $ script
  where 
    script = "#!/bin/bash \n" 
             <> "apt-get update \\\n"
             <> "  && apt-get install -y awscli netcat-traditional \\\n"
             <> "  && aws --region=" <> region <> " s3 cp s3://" <> bucket <> "/" <> s3Key <> " runMe \\\n"
             <> "  && chmod +x runMe \\\n"
             <> "  && echo \"Starting netcat...\" \\\n" 
             <> "  && nc.traditional -l -p " 
             <> (Text.pack $ show port) 
             <> " -c \"./runMe \" &"


-- This is currently not in use but I'm going to leave it since it worked well when I needed it.
-- I'm now assigning public addresses via NetworkInterfaceSpecification
-- https://hackage.haskell.org/package/amazonka-ec2-1.3.6/docs/Network-AWS-EC2-Types.html#v:inisAssociatePublicIPAddress
assignPublicAddress :: (MonadAWS m, MonadThrow m, MonadState AppState m, KatipContext m) => Instance -> m ()
assignPublicAddress i = do
  response <- send $ allocateAddress & aaDomain ?~ DTVPC 
  let publicIp     = response ^. aarsPublicIP
      allocationId = response ^. aarsAllocationId
      instanceId   = i ^. insInstanceId

  case allocationId of
    Nothing      -> throw CanNotGetPublicIPError
    Just allocId -> do
      -- Remember allocationId to be released later
      elasticIPs <>= [allocId]
      void . send $ associateAddress & aasInstanceId   ?~ instanceId
                                     & aasAllocationId .~ allocationId
      updateInstanceState



assignPublicIPAddressesIfNecessary :: ( MonadAWS m
                                      , MonadThrow m
                                      , MonadState AppState m
                                      , MonadReader AppConfig m
                                      , KatipContext m ) => m ()
assignPublicIPAddressesIfNecessary = do  
  isUsingPublicIps <- view $ configFile . awsUsePublicIP
  case isUsingPublicIps of
    False -> return ()
    True  -> do
      instances <- use ec2Instances
      INFO("Attempting to associate instances to public IPs")
      void $ mapM assignPublicAddress instances



cleanUpElasticIPs :: (MonadAWS m, MonadState AppState m) => m ()
cleanUpElasticIPs = use elasticIPs >>= mapM_ (\aid -> send $ releaseAddress & raAllocationId ?~ aid)



getStates :: DescribeInstanceStatusResponse -> [InstanceStateName] 
getStates r = r ^.. disrsInstanceStatuses . traverse . isInstanceState . _Just . isName



instanceStatuses :: (MonadAWS m) => [Text] -> m DescribeInstanceStatusResponse
instanceStatuses xs = send $ describeInstanceStatus & disInstanceIds .~ xs
                                                    & disIncludeAllInstances ?~ True



-- | Get states for instances
getInstanceStates :: (MonadAWS m, MonadState AppState m) => m [InstanceStateName]
getInstanceStates = getStates <$> (instanceStatuses =<< getInstanceIdsInState)



-- | Checks to see if the instances stored in local state have matching state
areInstancesAllState :: (MonadAWS m, MonadState AppState m) => InstanceStateName -> m Bool
areInstancesAllState stateName = areInstancesAllState' stateName <$> getInstanceStates
  where
    areInstancesAllState' stateName [] = False
    areInstancesAllState' stateName xs = all (stateName ==) xs


-- | Checks to see if the instances stored in local state are running
allInstancesAreReady :: forall m . (MonadAWS m, MonadState AppState m) => m Bool
allInstancesAreReady = areInstancesAllState ISNRunning



-- | Checks to see if the instances stored in local state are terminated
allInstancesAreTerminated :: forall m . (MonadAWS m, MonadState AppState m) => m Bool
allInstancesAreTerminated = areInstancesAllState ISNTerminated



-- | Gets the InstanceIds from the instances held in AppState
getInstanceIdsInState :: (MonadState AppState m) => m [Text]
getInstanceIdsInState = fmap (view insInstanceId) <$> use ec2Instances
    


updateInstanceState :: (MonadAWS m, MonadState AppState m, KatipContext m) => m ()
updateInstanceState = do
  instanceIds <- getInstanceIdsInState 
  response    <- send $ describeInstances & diiInstanceIds .~ instanceIds
  ec2Instances .= response ^. dirsReservations . traverse . rInstances



waitUntilInstancesAreRunning :: ( MonadAWS m
                                , MonadReader AppConfig m
                                , MonadState AppState m
                                , KatipContext m ) => m ()
waitUntilInstancesAreRunning = do
  allRunning <- allInstancesAreReady
  secs <- view $ configFile . waitToRunningSec

  if allRunning
  -- Added a second delay to prevent any timing errors with aws
  then void . liftIO $ threadDelay (1 * 1000000)

  else do
    $(logTM) InfoS "Instances are not ready... waiting..."
    liftIO $ threadDelay (secs * 1000000)
    waitUntilInstancesAreRunning



waitUntilInstancesAreTerminated :: ( MonadAWS m
                                   , MonadReader AppConfig m
                                   , MonadState AppState m
                                   , KatipContext m ) => m ()
waitUntilInstancesAreTerminated = do
  allTerminated <- allInstancesAreTerminated
  secs          <- view $ configFile . waitToRunningSec

  if allTerminated 
  then return ()
  else do
    INFO("Instances are not terminated... waiting...")
    liftIO $ threadDelay (secs * 1000000)
    waitUntilInstancesAreTerminated



createTagsOnInstances :: (MonadAWS m, MonadState AppState m) => SessionID -> m ()
createTagsOnInstances uid = do 
  instanceIds <- getInstanceIdsInState
  mapM_ createTagForInstanceId instanceIds
  where
    createTagForInstanceId instanceId = void $ do
      let instanceName = tag "Name" $ "Commander." <> uid <> "." <> instanceId
      send $ createTags & cResources <>~ [instanceId]
                        & cTags      <>~ [instanceName]


{-
 -- This is for making sure the generated userData looks formatted correctly
printUserData :: (MonadIO m, MonadReader AppConfig m, MonadState AppState m) => Text -> m ()
printUserData key = do
  bucket  <- view $ configFile . awsBucket
  port    <- view $ configFile . awsSGPort
  let script = userDataScript port bucket key
  void . liftIO . putDecoded . B64.decode . Text.encodeUtf8 $ script

  where
    putDecoded (Right bs) = Text.putStrLn . Text.decodeUtf8 $ bs
    putDecoded _          = return ()
-}


-- | Create instances to run jobs on
runInstanceWithScript :: ( MonadAWS m
                         , MonadIO m
                         , MonadReader AppConfig m
                         , MonadState AppState m
                         , KatipContext m ) => Text -> Text -> m ()
runInstanceWithScript scriptName scriptS3Key = do
  uuid    <- use sessionId
  sgId    <- getCommanderSecurityGroupId
  ami     <- view $ configFile . amiIdentifier
  snetId  <- view $ configFile . subnetIdentifier
  role    <- view $ configFile . iamRole
  keyName <- view $ configFile . keyPairName
  port    <- view $ configFile . awsSGPort
  public  <- view $ configFile . awsUsePublicIP
  bucket  <- view $ configFile . awsBucket
  itypeT  <- view $ configFile . instanceType
  region  <- view $ configFile . awsRegion

  let iamr    = iamInstanceProfileSpecification & iapsName ?~ role
      request = runInstances ami 1 1
      insType = either (const T1_Micro) id (fromText itypeT)
      spec    = instanceNetworkInterfaceSpecification & inisAssociatePublicIPAddress ?~ public
                                                      & inisSubnetId                 ?~ snetId
                                                      & inisGroups                  <>~ [ sgId ]
                                                      & inisDeviceIndex              ?~ 0

  INFO("Attempting to spin up instance")
  reservation <- send $ request & rNetworkInterfaces <>~ spec:[]
                                & rKeyName            ?~ keyName
                                & rIAMInstanceProfile ?~ iamr
                                & rInstanceType       ?~ insType
                                & rUserData           ?~ userDataScript port region bucket scriptS3Key

  INFO("Instance is starting up")
  ec2Instances <>= reservation ^. rInstances 



terminateInstancesInStateAndCleanUpElasticIPs :: ( MonadAWS m
                                                 , MonadIO m
                                                 , MonadState AppState m
                                                 , MonadReader AppConfig m
                                                 , KatipContext m ) => m ()
terminateInstancesInStateAndCleanUpElasticIPs = do
  INFO("Terminating Instances")
  instanceIds <- getInstanceIdsInState
  send $ terminateInstances & tiInstanceIds .~ instanceIds

  waitUntilInstancesAreTerminated

  INFO("Instances Terminated")
  ec2Instances .= mempty
  
  -- Clean up any elastic ips that might have been allocated
  usingPublic <- view $ configFile . awsUsePublicIP
  if not usingPublic then return ()
  else do
    cleanUpElasticIPs
    INFO("Instances Terminated")
