{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE TemplateHaskell     #-}

#include "../macros.h"

module Commander.Network where

import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue
import Control.Retry

import Data.ByteString
import Data.Maybe
import Data.Monoid

import Control.Lens 
import Control.Monad.STM
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Pipes
import Pipes.Safe
import Pipes.Network.TCP.Safe
import qualified Pipes.ByteString as PB

import Network.AWS
import Network.AWS.EC2

import Data.Text (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import Commander.Types
import Commander.Utils

import Katip

import System.IO
import GHC.IO.Exception

type Host' = String
type Port' = String



streamFromSocket :: TQueue (IO ()) -> Port' -> Host' -> IO ()
streamFromSocket q port host = do
  let retryStrategy = exponentialBackoff (10 * 1000000) <> limitRetries 5

  h <- openFile (host <> ".log") WriteMode
  recoverAll retryStrategy $ \status -> do
    atomically $ writeTQueue q $
      System.IO.hPutStrLn stderr $ host <> " :: Attempt number " <> (show (status ^. rsIterNumberL))
    runSafeT . runEffect $ fromConnect 4096 host port >-> PB.toHandle h



streamFromInstancesToFiles :: [Host'] -> Port' -> IO ()
streamFromInstancesToFiles hosts port = void $ do
  q <- newTQueueIO
  mapConcurrently (streamFromSocket q port) hosts



streamFromInstances :: (MonadState AppState m, MonadReader AppConfig m, MonadIO m, KatipContext m) => m ()
streamFromInstances = do
  instances <- use ec2Instances
  port'     <- view $ configFile . awsSGPort

  let ips :: [Host']
      ips = Text.unpack <$> (catMaybes $ getIPForInstance <$> instances)

      port :: Port'
      port = show port'

  INFO("Attempting to stream responses")
  liftIO $ streamFromInstancesToFiles ips port
