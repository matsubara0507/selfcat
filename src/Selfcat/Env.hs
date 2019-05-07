{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Selfcat.Env where

import           RIO

import           Data.Extensible
import           Mix.Plugin.Logger ()
import           Selfcat.Config

type Env = Record
  '[ "config" >: Config
   , "output" >: FilePath
   , "github" >: ByteString
   , "logger" >: LogFunc
   ]
