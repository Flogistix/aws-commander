{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
module Commander.Network where

import Control.Lens 
import Control.Monad.Reader
import Control.Monad.Except

import Network.Socket

import Pipes
import Pipes.Network.TCP hiding (connect)

import Network.AWS
import Network.AWS.EC2

import Data.ByteString

import Data.Text (Text)
import qualified Data.Text as Text

import Commander.EC2 
import Commander.Types



streamFromSocket :: (MonadIO m, MonadError NetworkError m, MonadReader AppConfig m) => Instance -> Producer' ByteString m ()
streamFromSocket inst = do
  case getIPForInstance inst of
    Nothing -> return ();
    Just ip -> do
      host      <- return . Text.unpack $ ip
      port      <- view $ configFile . awsSGPort
      addrInfos <- liftIO $ getAddrInfo Nothing (Just host) (Just . show $ port)
      sock      <- tryToConnectSocket addrInfos
      fromSocket sock 4096

  where
    tryToConnectSocket :: (MonadIO m, MonadError NetworkError m) => [AddrInfo] -> m Socket  
    tryToConnectSocket []     = throwError CanNotResolveAddressError
    tryToConnectSocket (x:xs) = do
      s <- liftIO $ socket (addrFamily x) Stream defaultProtocol
      liftIO $ connect s (addrAddress x)
      return s

streamFromInstancesToFiles :: (MonadIO m) => m ()
streamFromInstancesToFiles = undefined
