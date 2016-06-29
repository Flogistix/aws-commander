{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
module Commander.Network where

import Control.Applicative
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

import Commander.Types

getIPForInstance :: (MonadReader AppConfig m, MonadIO m, MonadError NetworkError m) => Instance -> m Text
getIPForInstance inst = do
  ip <- return $ inst ^. insPublicIPAddress <|> inst ^. insPrivateIPAddress
  case ip of
    Nothing -> throwError NoIPAddressesAvailableError
    Just ip -> return ip


streamFromSocket :: (MonadIO m, MonadError NetworkError m, MonadReader AppConfig m) => Instance -> Producer' ByteString m ()
streamFromSocket inst = do
  host      <- Text.unpack <$> getIPForInstance inst
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
