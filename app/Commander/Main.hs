{-# LANGUAGE OverloadedStrings #-}
module Main where

import Commander

import System.Console.GetOpt
import System.Exit
import System.Environment
import System.IO

data Flags = Flags { configFile :: Maybe String
                   , scriptDir :: Maybe String
                   }

options :: [ OptDescr (Flags -> IO Flags) ]
options = [ Option ['c'] ["config"]
          ( OptArg (\x opts -> return $ opts { configFile = x }) "path/to/commander.conf" )
          "Use commander configuration file at alternate location"
 
          , Option ['s'] ["script"]
          ( OptArg (\x opts -> return $ opts { scriptDir = x }) "path/to/scripts/" )
          "Use alternate script directory"
          
          , Option ['h'] ["help"]
          ( NoArg (\_ -> do
              name <- getProgName
              hPutStrLn stderr (usageInfo name options)
              exitSuccess
            )
          )
          "Show help message"
          ]

main :: IO ()
main = do
  args <- getArgs
  let (actions, nonOpts, errors) = getOpt RequireOrder options args

  opts <- foldl (>>=) (return $ Flags Nothing Nothing) actions

  case scriptDir opts of
    Nothing -> runCommanderWithScriptDirectory "scripts"
    Just x  -> runCommanderWithScriptDirectory x


