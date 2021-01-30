{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Selfcat.Cmd where

import           RIO
import qualified RIO.Text                 as Text
import qualified RIO.Text.Lazy            as TL

import           Data.Aeson               (ToJSON)
import qualified Data.Aeson.Encode.Pretty as Json
import           Data.Extensible
import qualified Data.Text.Lazy.Builder   as TL
import           GitHub.Data.Name         (mkName)
import qualified GitHub.Endpoints.Repos   as GitHub
import qualified GitHub.Endpoints.Users   as GitHub
import qualified Mix.Plugin.GitHub        as MixGitHub
import           Selfcat.Env
import           Selfcat.Info

cmd :: RIO Env ()
cmd = do
  config <- asks (view #config)
  buildUserJson $ config ^. #owner
  mapM_ (buildRepoJson $ config ^. #owner) (config ^. #repos)
  logInfo "Success."

cmdCompact :: RIO Env ()
cmdCompact = do
  owner <- asks (view #owner . view #config)
  fetchUserInfo owner >>= \case
    Nothing   -> logError $ display $ mconcat ["can't build user info: ", owner]
    Just user -> do
      repoNames <- asks (view #repos . view #config)
      repos <- catMaybes <$> mapM (fetchRepoInfo owner) repoNames
      buildJson Nothing $ #owner @= user <: #repos @= repos <: emptyRecord
      logInfo "Success."

buildUserJson :: Text -> RIO Env ()
buildUserJson name =
  maybe mempty (buildJson $ Just name) =<< fetchUserInfo name

buildRepoJson :: Text -> Text -> RIO Env ()
buildRepoJson owner name =
  maybe mempty (buildJson $ Just name) =<< fetchRepoInfo owner name

buildJson :: (Show r, ToJSON r) => Maybe Text -> r -> RIO Env ()
buildJson name info = do
  logDebug $ display $ mconcat ["get: ", tshow info]
  path <- (++ filename) <$> asks (view #output)
  logDebug $ fromString $ mconcat ["build: ", path]
  writeFileUtf8 path $
    TL.toStrict (TL.toLazyText $ Json.encodePrettyToTextBuilder info)
  where
    filename = maybe "" (\t -> "/" ++ Text.unpack t ++ ".json") name

fetchUserInfo :: Text -> RIO Env (Maybe UserInfo)
fetchUserInfo name = do
  logDebug $ display $ mconcat ["fetch github: ", name]
  resp <- MixGitHub.fetch $ GitHub.userInfoForR
    (mkName Proxy name)
  case resp of
    Left err   -> logError (displayShow err) >> pure Nothing
    Right user -> pure $ Just (toInfo user)

fetchRepoInfo :: Text -> Text -> RIO Env (Maybe RepoInfo)
fetchRepoInfo owner name = do
  logDebug $ display $ mconcat ["fetch github: ", owner, "/", name]
  resp <- MixGitHub.fetch $ GitHub.repositoryR
    (mkName Proxy owner)
    (mkName Proxy name)
  case resp of
    Left err   -> logError (displayShow err) >> pure Nothing
    Right repo -> pure $ Just (toInfo repo)

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
