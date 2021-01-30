{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}

module Selfcat.Info where

import           RIO

import           Data.Extensible
import qualified GitHub.Data.Definitions as GitHub
import           GitHub.Data.Name        (untagName)
import qualified GitHub.Data.Repos       as GitHub
import           GitHub.Data.URL         (getUrl)

type Info = Record
  '[ "owner" >: UserInfo
   , "repos" >: [RepoInfo]
   ]

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
     <: #fork_cnt    @= GitHub.repoForksCount repo
     <: #language    @= maybe "" GitHub.getLanguage (GitHub.repoLanguage repo)
     <: nil
