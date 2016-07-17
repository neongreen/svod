-- |
-- Module      :  Handler.User.Followers
-- Copyright   :  © 2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Serve list of followers of particular user.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.User.Followers
  ( getUserFollowersR )
where

import Data.Foldable (null)
import Data.Pagination
import Helper.Access (userViaSlug)
import Helper.Json (userJson, paginatedJson)
import Import hiding (null)
import Widget.Pagination (lookupPagination, paginationW)
import Widget.User (userW)
import qualified Svod as S

-- | Serve list of followers of particular user.

getUserFollowersR :: Slug -> Handler TypedContent
getUserFollowersR slug = userViaSlug slug $ \user -> do
  let uid = entityKey user
      User {..} = entityVal user
  params    <- lookupPagination
  paginated <- runDB (S.followersPaginated params uid)
  selectRep $ do
    -- HTML representation
    provideRep . noHeaderLayout $ do
      let title = toHtml (userName <> " (подписчики)")
      setTitle title
      $(widgetFile "user-followers")
    -- JSON representation
    provideRep $ do
      render <- getUrlRender
      fmap paginatedJson . forM paginated $ \user' -> do
        let (Entity uid' u) = user'
        followers <- runDB (S.followerCount uid')
        return (userJson render followers u)
