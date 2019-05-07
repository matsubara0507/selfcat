{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Selfcat.Config where

import           RIO

import           Data.Extensible
import qualified Data.Yaml       as Y

type Config = Record
  '[ "owner" >: Text
   , "repos" >: [Text]
   ]

readConfig :: MonadIO m => FilePath -> m Config
readConfig = Y.decodeFileThrow
