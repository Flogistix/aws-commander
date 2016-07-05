{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs        #-}
module Commander.Network where

import Control.Concurrent.Async
import Control.Retry

import Data.ByteString
import Data.Maybe
import Data.Monoid

import Control.Lens 
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

import System.IO
import GHC.IO.Exception

type Host' = String
type Port' = String

streamFromSocket :: Port' -> Host' -> IO ()
streamFromSocket port host = do
  h <- openFile (host <> ".log") WriteMode
  runSafeT . runEffect $ fromConnect 4096 host port >-> PB.toHandle h

streamFromInstancesToFiles :: [Host'] -> Port' -> IO ()
streamFromInstancesToFiles hosts port = void $ mapConcurrently (streamFromSocket port) hosts

streamFromInstances :: (MonadState AppState m, MonadReader AppConfig m, MonadIO m) => m ()
streamFromInstances = do
  instances <- use ec2Instances
  port'     <- view $ configFile . awsSGPort

  let ips :: [Host']
      ips = Text.unpack <$> (catMaybes $ getIPForInstance <$> instances)

      port :: Port'
      port = show port'

  liftIO $ Text.putStrLn "Got here"
  liftIO $ mapM Prelude.putStrLn ips
  liftIO $ Prelude.putStrLn port

  liftIO $ recoverAll (exponentialBackoff (10 * 1000000)) $ \status -> do
    Prelude.putStrLn . show $ status
    streamFromInstancesToFiles ips port
