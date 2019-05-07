{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}

module Selfcat.Cmd where

import           RIO
import qualified RIO.Text                 as Text
import qualified RIO.Text.Lazy            as TL

import           Data.Aeson               (ToJSON)
import qualified Data.Aeson.Encode.Pretty as Json
import           Data.Extensible
import qualified Data.Text.Lazy.Builder   as TL
import qualified GitHub.Data.Definitions  as GitHub
import           GitHub.Data.Name         (mkName, untagName)
import qualified GitHub.Data.Repos        as GitHub
import           GitHub.Data.URL          (getUrl)
import qualified GitHub.Endpoints.Repos   as GitHub
import qualified GitHub.Endpoints.Users   as GitHub
import qualified Mix.Plugin.GitHub        as MixGitHub
import           Selfcat.Env

cmd :: RIO Env ()
cmd = do
  config <- asks (view #config)
  buildUserJson $ config ^. #owner
  mapM_ (buildRepoJson $ config ^. #owner) (config ^. #repos)
  logInfo "Success."

type UserInfo = Record
  '[ "login"     >: Text
   , "name"      >: Maybe Text
   , "avatar"    >: Text
   , "url"       >: Text
   , "followers" >: Int
   , "following" >: Int
   , "repo_cnt"  >: Int
   , "bio"       >: Text
   ]

type RepoInfo = Record
  '[ "name"        >: Text
   , "owner"       >: Text
   , "avatar"      >: Text
   , "url"         >: Text
   , "description" >: Text
   , "star_cnt"    >: Int
   , "fork_cnt"    >: Int
   , "language"    >: Text
   ]

buildUserJson :: Text -> RIO Env ()
buildUserJson name = do
  logDebug $ display $ mconcat [ "fetch github: ", name ]
  resp <- MixGitHub.fetch $ \auth -> GitHub.userInfoFor' (Just auth)
    (mkName Proxy name)
  case resp of
    Left err   -> logError $ displayShow err
    Right user -> buildJson name user

buildRepoJson :: Text -> Text -> RIO Env ()
buildRepoJson owner name = do
  logDebug $ display $ mconcat [ "fetch github: ", owner, "/", name ]
  resp <- MixGitHub.fetch $ \auth -> GitHub.repository' (Just auth)
    (mkName Proxy owner)
    (mkName Proxy name)
  case resp of
    Left err   -> logError $ displayShow err
    Right repo -> buildJson name repo

buildJson :: forall a r . (Show r, ToJSON r, ToInfo a r) => Text -> a -> RIO Env ()
buildJson name dat = do
  let info = toInfo dat :: r
  logDebug $ display $ mconcat [ "get: ", tshow info ]
  path <- (++ "/" ++ Text.unpack name ++ ".json") <$> asks (view #output)
  logDebug $ fromString $ mconcat [ "build: ", path ]
  writeFileUtf8 path $ TL.toStrict $ TL.toLazyText $ Json.encodePrettyToTextBuilder info

class ToInfo a r | a -> r where
  toInfo :: a -> r

instance ToInfo GitHub.User UserInfo where
  toInfo user
      = #login     @= untagName (GitHub.userLogin user)
     <: #name      @= GitHub.userName user
     <: #avatar    @= getUrl (GitHub.userAvatarUrl user)
     <: #url       @= getUrl (GitHub.userHtmlUrl user)
     <: #followers @= GitHub.userFollowers user
     <: #following @= GitHub.userFollowing user
     <: #repo_cnt  @= GitHub.userPublicRepos user
     <: #bio       @= fromMaybe "" (GitHub.userBio user)
     <: nil

instance ToInfo GitHub.Repo RepoInfo where
  toInfo repo
      = #name        @= untagName (GitHub.repoName repo)
     <: #owner       @= untagName (GitHub.simpleOwnerLogin $ GitHub.repoOwner repo)
     <: #avatar      @= getUrl (GitHub.simpleOwnerAvatarUrl $ GitHub.repoOwner repo)
     <: #url         @= getUrl (GitHub.repoHtmlUrl repo)
     <: #description @= fromMaybe "" (GitHub.repoDescription repo)
     <: #star_cnt    @= GitHub.repoStargazersCount repo
     <: #fork_cnt    @= fromMaybe 0 (GitHub.repoForks repo)
     <: #language    @= maybe "" GitHub.getLanguage (GitHub.repoLanguage repo)
     <: nil

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
