{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Paths_selfcat          (version)
import           RIO
import qualified RIO.ByteString         as B

import           Configuration.Dotenv   (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Version           (Version)
import qualified Data.Version           as Version
import           Development.GitRev
import           Mix
import           Mix.Plugin.GitHub      as MixGitHub
import           Mix.Plugin.Logger      as MixLogger
import           Selfcat.Cmd
import           Selfcat.Config
import           System.Environment     (getEnv)

main :: IO ()
main = withGetOpt "[options] [input-file]" opts $ \r args -> do
  _ <- tryIO $ loadFile defaultConfig
  case (r ^. #version, listToMaybe args) of
    (True, _)      -> B.putStr $ fromString (showVersion version) <> "\n"
    (_, Nothing)   -> error "please input config file path."
    (_, Just path) -> runCmd r path
  where
    opts = #version @= versionOpt
        <: #verbose @= verboseOpt
        <: #output  @= outputOpt
        <: nil

type Options = Record
  '[ "version" >: Bool
   , "verbose" >: Bool
   , "output"  >: FilePath
   ]

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

outputOpt :: OptDescr' FilePath
outputOpt = optionReqArg f ['o'] ["output"] "PATH" "Directory path for outputs"
  where
    defaultValue = "."
    f = pure . fromMaybe defaultValue . listToMaybe

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]

runCmd :: Options -> FilePath -> IO ()
runCmd opts path = do
  config <- readConfig path
  token  <- liftIO $ fromString  <$> getEnv "GH_TOKEN"
  let logOpts = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
      plugin  = hsequence
          $ #config <@=> pure config
         <: #output <@=> pure (opts ^. #output)
         <: #github <@=> MixGitHub.buildPlugin token
         <: #logger <@=> MixLogger.buildPlugin logOpts
         <: nil
  Mix.run plugin cmd
