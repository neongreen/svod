-- |
-- Module      :  Handler.User.Followers
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Serve list of followers of particular user.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.User.Followers
  ( getUserFollowersR )
where

import Helper.Access (userViaSlug)
import Helper.Json (userJson, paginatedJson)
import Import
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
    provideRep . defaultLayout $ do
      setTitle (toHtml $ userName <> " (подписчики)")
      $(widgetFile "user-followers")
    -- JSON representation
    provideRep $ do
      render <- getUrlRender
      items  <- forM (S.paginatedItems paginated) $ \user' -> do
        let uid' = entityKey user'
            val' = entityVal user'
        followers <- runDB (S.followerCount uid')
        return (userJson render followers val')
      return (paginatedJson $ paginated { S.paginatedItems = items })
