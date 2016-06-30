{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}

#include "../../macros.h"

module Commander.EC2.SecurityGroup where

import Control.Concurrent
import Control.Lens
import Control.Monad.Reader

import Data.Maybe

import Network.AWS
import Network.AWS.EC2

import Data.Text (Text)
import qualified Data.Text as Text

import Katip

import Commander.Types

commanderSGName = "CommanderSG"
commanderSGDesc = "Commander SG"



getVPCIdFromSubnet :: (MonadAWS m, MonadReader AppConfig m, KatipContext m) => Text -> m (Maybe Text)
getVPCIdFromSubnet snId = do
  INFO("Getting VPC assigned to subnet")
  response <- send $ describeSubnets & dsSubnetIds .~ [snId]
  return . listToMaybe $ response ^.. dsrsSubnets . traverse . subVPCId



checkToSeeIfSecurityGroupIdExists :: (MonadAWS m, KatipContext m) => Text -> m Bool
checkToSeeIfSecurityGroupIdExists groupId = do
  response <- send $ describeSecurityGroups & dsgsGroupIds .~ [groupId]
  return $ (length $ response ^. dsgrsSecurityGroups) > 0



checkToSeeIfSecurityGroupNameExists :: (MonadAWS m, KatipContext m) => Text -> m (Maybe SecurityGroup)
checkToSeeIfSecurityGroupNameExists name = do
  INFO("Checking to see if security group exists")
  response <- send $ describeSecurityGroups
  let sgroups :: [SecurityGroup]
      sgroups = response ^. dsgrsSecurityGroups
  return . maybeGetMatching $ sgroups
  where
    maybeGetMatching = listToMaybe . filter (\sg -> name == sg ^. sgGroupName)



waitForSG :: (MonadAWS m, KatipContext m) => Text -> m ()
waitForSG sgId = do
  sgExists <- checkToSeeIfSecurityGroupIdExists sgId
  case sgExists of
    True  -> return ()
    False -> do
      INFO("SG creation in progress... waiting 3 seconds...")
      liftIO $ threadDelay (3 * 1000000)
      waitForSG sgId



createCommanderSecurityGroup :: (MonadAWS m, MonadReader AppConfig m, KatipContext m) => m Text
createCommanderSecurityGroup = do
  INFO("Attempting to create security group")

  snetId   <- view $ configFile . subnetIdentifier
  vpcId    <- getVPCIdFromSubnet snetId
  response <- send $ createSecurityGroup commanderSGName commanderSGDesc & csgVPCId .~ vpcId

  let groupId = response ^. csgrsGroupId

  -- Wait for Security group to finish creating in AWS 
  waitForSG groupId
  INFO("Security Group created")

  -- Then assign correct port/network info
  attachIPPermissions groupId

  return groupId



attachIPPermissions :: (MonadAWS m, KatipContext m, MonadReader AppConfig m) => Text -> m ()
attachIPPermissions groupId = do
  cidr     <- view $ configFile . awsSGCidr
  port     <- view $ configFile . awsSGPort

  INFO("Assigning IP permissions")
  send $ authorizeSecurityGroupIngress & asgiGroupId       ?~ groupId
                                       & asgiFromPort      ?~ port
                                       & asgiToPort        ?~ port
                                       & asgiCIdRIP        ?~ cidr
                                       & asgiIPProtocol    ?~ "tcp"

  INFO("IP permissions assigned")
  return ()



getCommanderSecurityGroupId :: (MonadAWS m, MonadReader AppConfig m, KatipContext m) => m Text
getCommanderSecurityGroupId = createGroupIfNotExists =<< checkToSeeIfSecurityGroupNameExists commanderSGName 
  where
    createGroupIfNotExists :: (MonadAWS m, MonadReader AppConfig m, KatipContext m) => Maybe SecurityGroup -> m Text
    createGroupIfNotExists (Just sg) = do
      INFO("Security Group exists")
      return $ sg ^. sgGroupId

    createGroupIfNotExists _         = do
      INFO("Security Group does not exist")
      createCommanderSecurityGroup
