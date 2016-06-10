-- |
-- Module      :  Handler.User.Stars
-- Copyright   :  © 2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Serve list of releases user starred.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.User.Stars
  ( getUserStarsR )
where

import Data.Maybe (fromJust)
import Helper.Access (userViaSlug)
import Helper.Json (releaseJson, paginatedJson)
import Import
import Widget.Pagination (lookupPagination, paginationW)
import Widget.Release (releaseW)
import qualified Svod as S

-- | Serve list of releases user starred.

getUserStarsR :: Slug -> Handler TypedContent
getUserStarsR slug = userViaSlug slug $ \user -> do
  let uid = entityKey user
  params    <- lookupPagination
  paginated <- runDB (S.starredPaginated params uid)
  selectRep $ do
    -- HTML representation
    provideRep . defaultLayout $ do
      setTitle "Отмеченные публикации"
      $(widgetFile "user-stars")
    -- JSON representation
    provideRep $ do
      render <- getUrlRender
      fmap paginatedJson . forM paginated $ \release -> do
        let (Entity rid r@Release {..}) = release
        u        <- fromJust <$> runDB (get releaseArtist)
        starrers <- runDB (S.starCount rid)
        return (releaseJson render starrers u r)
