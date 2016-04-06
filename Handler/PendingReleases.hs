-- |
-- Module      :  Handler.PendingReleases
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Serve list of pending releases accessible only to admins and staff.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.PendingReleases
  ( getPendingReleasesR )
where

import Data.Maybe (fromJust)
import Helper.Auth
import Helper.Json (releaseJson, paginatedJson)
import Import
import Widget.Pagination (lookupPagination, paginationW)
import Widget.Release (releaseW)
import qualified Svod as S

-- | Serve list of pending releases accessible only to admins and staff.

getPendingReleasesR :: Handler TypedContent
getPendingReleasesR = do
  checkAuthWith isStaff
  params    <- lookupPagination
  paginated <- runDB (S.getPendingReleases params)
  selectRep $ do
    -- HTML representation
    provideRep . defaultLayout $ do
      setTitle "Висящие публикации"
      $(widgetFile "pending-releases")
    -- JSON representation
    provideRep $ do
      render <- getUrlRender
      fmap paginatedJson . forM paginated $ \release -> do
        let (Entity rid r@Release {..}) = release
        u        <- fromJust <$> runDB (get releaseArtist)
        starrers <- runDB (S.starCount rid)
        return (releaseJson render starrers u r)
