{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleInstances          #-}

module Commander.Types where

import Data.Typeable

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch
import Control.Monad.Signatures

import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource

import Control.Lens
import Control.Lens.TH

import Control.Exception (Exception)

import Katip

import Network.AWS
import Network.AWS.EC2

import Data.Text (Text)
import qualified Data.Text as Text

type SessionID  = Text
type ScriptName = String

data ConfigError = NoConfigurationFilesFoundError
                 | ConfigurationCouldNotParseError Text
  deriving (Show)

data NetworkError = CanNotResolveAddressError
                  | NoIPAddressesAvailableError
  deriving (Show)

data CommanderError = CanNotGetPublicIPError
  deriving (Show, Typeable)
instance Exception CommanderError


data ConfigFile = ConfigFile { _awsRegion           :: Text
                             , _awsBucket           :: Text
                             , _maxInstances        :: Int
                             , _waitToRunningSec    :: Int
                             , _keyPairName         :: Text
                             , _amiIdentifier       :: Text
                             , _subnetIdentifier    :: Text
                             , _awsSGCidr           :: Text
                             , _awsSGPort           :: Int
                             , _awsUsePublicIP      :: Bool
                             , _instanceType        :: Text
                             , _iamRole             :: Text
                             } deriving (Show, Read)
makeLenses ''ConfigFile


data AppConfig = AppConfig { _configFile :: ConfigFile } deriving (Show)
makeLenses ''AppConfig


data AppState = AppState { _ec2Instances   :: [Instance]
                         , _sessionId      :: Text
                         , _katipContext   :: LogContexts
                         , _katipLogEnv    :: LogEnv
                         , _katipNamespace :: Namespace
                         , _elasticIPs     :: [Text]
                         , _scriptsToRun   :: [Text]
                         }
makeLenses ''AppState


newtype CommanderT m a = CommanderT { unStack :: ReaderT AppConfig (StateT AppState m) a  }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadReader AppConfig, MonadState AppState
           , MonadCatch, MonadThrow, MonadAWS, MonadMask )


instance MonadIO m => Katip (CommanderT m) where
  getLogEnv = use katipLogEnv


instance MonadIO m => KatipContext (CommanderT m) where
  getKatipContext   = use katipContext
  getKatipNamespace = use katipNamespace


runCommander :: MonadIO m => AppConfig -> AppState -> CommanderT m a -> m a
runCommander c s = (flip evalStateT) s . (flip runReaderT) c . unStack

