{-# LANGUAGE OverloadedStrings #-}
module Main where

import Commander

main :: IO ()
main = do
  let scriptDir = "scripts"
  runCommanderWithScriptDirectory scriptDir
