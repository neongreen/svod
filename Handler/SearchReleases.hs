-- |
-- Module      :  Handler.SearchReleases
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- General searching facility.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.SearchReleases
  ( getSearchReleasesR )
where

import Data.Maybe (fromJust)
import Helper.Json (releaseJson, paginatedJson)
import Import
import Widget.Pagination (lookupPagination, paginationW)
import Widget.Release (releaseW)
import Widget.Search (searchW)
import qualified Svod as S

-- | General searching facility.

getSearchReleasesR :: Handler TypedContent
getSearchReleasesR = do
  params    <- lookupPagination
  paginated <- runDB (S.releaseQuery params [] []) -- FIXME
  selectRep $ do
    -- HTML representation
    provideRep . noHeaderLayout $ do
      setTitle "Поиск публикаций"
      $(widgetFile "search-releases")
    -- JSON representation
    provideRep $ do
      render <- getUrlRender
      items  <- forM (S.paginatedItems paginated) $ \release -> do
        let (Entity rid r@Release {..}) = release
        stars <- runDB (S.starCount rid)
        u     <- fromJust <$> runDB (get releaseArtist)
        return (releaseJson render stars u r)
      return (paginatedJson $ paginated { S.paginatedItems = items })
