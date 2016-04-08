-- |
-- Module      :  Handler.Release.Starrers
-- Copyright   :  © 2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Serve list of users who have starred particular release (determined by
-- URL).

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Release.Starrers
  ( getReleaseStarrersR )
where

import Helper.Access (releaseViaSlug)
import Helper.Json (userJson, paginatedJson)
import Import
import Widget.Pagination (lookupPagination, paginationW)
import Widget.User (userW)
import qualified Svod as S

-- | Serve list of users who have starred particular release.

getReleaseStarrersR :: Slug -> Slug -> Handler TypedContent
getReleaseStarrersR uslug rslug = releaseViaSlug uslug rslug $ \_ release -> do
  let rid = entityKey release
      Release {..} = entityVal release
  params    <- lookupPagination
  paginated <- runDB (S.starredByPaginated params rid)
  selectRep $ do
    -- HTML representation
    provideRep . noHeaderLayout $ do
      let title = toHtml (releaseTitle <> " (кто отметил)")
      setTitle title
      $(widgetFile "release-starrers")
    -- JSON representation
    provideRep $ do
      render <- getUrlRender
      fmap paginatedJson . forM paginated $ \user -> do
        let (Entity uid u) = user
        followers <- runDB (S.followerCount uid)
        return (userJson render followers u)
