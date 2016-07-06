{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Commander.S3 where

import Data.Conduit.Binary (sourceFile)
import Data.Monoid
import Data.List

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import Network.AWS
import Network.AWS.Data.Body
import Network.AWS.S3
import Katip

import Data.Text (Text)
import qualified Data.Text as Text

import System.IO
import System.Posix

import Commander.Types

getLocalFileSize :: FilePath -> IO Integer
getLocalFileSize = fmap (fromIntegral . fileSize) . getFileStatus

uploadScript :: (MonadAWS m, MonadReader AppConfig m, MonadState AppState m) => Text -> FilePath -> m ()
uploadScript name path = do
  bucket <- view $ configFile . awsBucket
  body   <- makeRequestBody path

  let getLastElementOfPath :: FilePath -> Text
      getLastElementOfPath  = Text.concat . take 1 . reverse . Text.splitOn "/" . Text.pack

      objectKey :: Text
      objectKey = name <> "-" <> (getLastElementOfPath path)

  send $ putObject (BucketName bucket) (ObjectKey objectKey) body
  scriptsToRun <>= objectKey:[]

  where
    makeRequestBody :: MonadIO m => FilePath -> m RqBody
    makeRequestBody path = do
      fsize <- liftIO $ getLocalFileSize path
      return $ Chunked (ChunkedBody defaultChunkSize fsize $ sourceFile path)



uploadScripts :: ( MonadAWS m
                 , MonadState AppState m
                 , MonadReader AppConfig m
                 , KatipContext m ) => Text -> [FilePath] -> m ()
uploadScripts name = mapM_ (uploadScript name)
