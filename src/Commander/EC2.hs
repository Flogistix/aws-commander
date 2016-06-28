{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs        #-}
module Commander.EC2 where

import Data.List
import Data.Monoid

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Lens.Prism
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State

import Control.Monad.Trans.Error
import Control.Monad.Trans.Resource

import Data.Text (Text)
import qualified Data.Text.Encoding  as Text
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text

import System.IO

import Network.AWS
import Network.AWS.EC2

import Katip

import Commander.Conf
import Commander.Types
import Commander.EC2.SecurityGroup

import qualified Data.ByteString.Base64 as B64

import Debug.Trace

userDataScript :: Int -> Text
userDataScript port = Text.decodeUtf8 . B64.encode . Text.encodeUtf8 $ script
  where 
    script = "#!/bin/bash \
             \ apt-get install -y netcat-traditional && \
             \ echo \"Starting netcat...\" && \
             \ nc.traditional -l -p " <> (Text.pack $ show port) <> " -c \"/bin/date\" &"



assignPublicAddress :: (MonadAWS m) => Instance -> m ()
assignPublicAddress i = do
  response <- send $ allocateAddress & aaDomain ?~ DTVPC 
  let publicIp     = response ^. aarsPublicIP
      allocationId = response ^. aarsAllocationId
      instanceId   = i ^. insInstanceId

  case allocationId of
    Nothing    -> return ()
    Just    _  -> do
      void . send $ associateAddress & aasInstanceId   ?~ instanceId
                                     & aasAllocationId .~ allocationId



assignPublicIPAddresses :: (MonadAWS m, MonadState AppState m, MonadReader AppConfig m, KatipContext m) => m ()
assignPublicIPAddresses = do  
  isUsingPublicIps <- view $ configFile . awsUsePublicIP
  case isUsingPublicIps of
    False -> return ()
    True  -> do
      instances <- use ec2Instances
      $(logTM) InfoS "Attempting to associate instances to public IPs"
      void $ mapM assignPublicAddress instances



-- | Checks to see if the instances stored in local state are running
allInstancesAreReady :: forall m . (MonadAWS m, MonadState AppState m) => m Bool
allInstancesAreReady = areInstancesRunning . getStates <$> (instanceStatuses =<< getInstanceIdsInState)
  where
    areInstancesRunning [] = False
    areInstancesRunning xs = all (ISNRunning ==) xs

    getStates :: DescribeInstanceStatusResponse -> [InstanceStateName] 
    getStates r = r ^.. disrsInstanceStatuses . traverse . isInstanceState . _Just . isName

    instanceStatuses :: [Text] -> m DescribeInstanceStatusResponse
    instanceStatuses xs = send $ describeInstanceStatus & disInstanceIds .~ xs



-- | Gets the InstanceIds from the instances held in AppState
getInstanceIdsInState :: (MonadState AppState m) => m [Text]
getInstanceIdsInState = fmap (view insInstanceId) <$> use ec2Instances
    


getInstanceState :: Instance -> InstanceStateName
getInstanceState i = i ^. (insState . isName)



isInstanceStateEq :: InstanceStateName -> Instance -> Bool
isInstanceStateEq s = (s ==) . getInstanceState



updateInstances :: (MonadAWS m, MonadState AppState m, KatipContext m) => m ()
updateInstances = do
  instanceIds <- getInstanceIdsInState 
  response    <- send $ describeInstances & diiInstanceIds .~ instanceIds
  ec2Instances .= response ^. dirsReservations . traverse . rInstances



waitUntilInstancesAreRunning :: (MonadAWS m, MonadReader AppConfig m, MonadState AppState m, KatipContext m) => m ()
waitUntilInstancesAreRunning = do
  allRunning <- allInstancesAreReady
  secs <- view $ configFile . waitToRunningSec
  if allRunning then return ()
  else do
    $(logTM) InfoS "Instances are not ready... waiting..."
    liftIO $ threadDelay (secs * 1000000)
    waitUntilInstancesAreRunning



-- | Create instances to run jobs on. Install Pleb.
createInstances :: (MonadAWS m, MonadIO m, MonadReader AppConfig m, MonadState AppState m, KatipContext m) => m ()
createInstances = do
  uuid    <- use sessionId
  sgId    <- getCommanderSecurityGroupId
  ami     <- view $ configFile . amiIdentifier
  snetId  <- view $ configFile . subnetIdentifier
  role    <- view $ configFile . iamRole
  keyName <- view $ configFile . keyPairName
  port    <- view $ configFile . awsSGPort
  num     <- view $ configFile . numberOfInstances

  let iamr    = iamInstanceProfileSpecification & iapsName ?~ role
      request = runInstances ami num num

  liftIO . Text.putStrLn $ userDataScript port

  $(logTM) InfoS "Attempting to spin up instances"
  reservation <- send $ request & rSecurityGroupIds  <>~ [sgId]
                                & rKeyName            ?~ keyName
                                & rSubnetId           ?~ snetId
                                & rIAMInstanceProfile ?~ iamr
                                & rUserData           ?~ userDataScript port

  $(logTM) InfoS "Instances are starting up"
  ec2Instances .= reservation ^. rInstances 
  waitUntilInstancesAreRunning
  updateInstances
  -- Need to give each instance a name tag



terminateInstancesInState :: (MonadAWS m, MonadIO m, MonadState AppState m, KatipContext m) => m ()
terminateInstancesInState = do
  instanceIds <- getInstanceIdsInState
  send $ terminateInstances & tiInstanceIds .~ instanceIds

  -- clear instance state
  ec2Instances .= mempty
