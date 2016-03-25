-- |
-- Module      :  Handler.User.Following
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Serve list of users specific user is following.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.User.Following
  ( getUserFollowingR )
where

import Helper.Access (userViaSlug)
import Helper.Json (userJson, paginatedJson)
import Import
import Widget.Pagination (lookupPagination, paginationW)
import Widget.User (userW)
import qualified Svod as S

-- | Serve list of users specific user is following.

getUserFollowingR :: Slug -> Handler TypedContent
getUserFollowingR slug = userViaSlug slug $ \user -> do
  let uid = entityKey user
      User {..} = entityVal user
  params    <- lookupPagination
  paginated <- runDB (S.followingPaginated params uid)
  selectRep $ do
    -- HTML representation
    provideRep . defaultLayout $ do
      setTitle (toHtml $ userName <> " (подписки)")
      $(widgetFile "user-following")
    -- JSON representation
    provideRep $ do
      render <- getUrlRender
      items  <- forM (S.paginatedItems paginated) $ \user' -> do
        let (Entity uid' u) = user'
        followers <- runDB (S.followingCount uid')
        return (userJson render followers u)
      return (paginatedJson $ paginated { S.paginatedItems = items })
