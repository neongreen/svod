-- |
-- Module      :  Handler.SearchReleases
-- Copyright   :  © 2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- General searching facility.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.SearchReleases
  ( getSearchReleasesR )
where

import Data.Maybe (fromJust)
import Helper.Json (releaseJson, paginatedJson)
import Import
import Svod.Search
import Widget.Pagination (lookupPagination, paginationW)
import Widget.Release (releaseW)
import Widget.Search (searchW, searchWidgetQueryParam)
import qualified Svod as S

-- | General searching facility.

getSearchReleasesR :: Handler TypedContent
getSearchReleasesR = do
  -- Check if we've got raw search query from user.
  searchInput <- lookupGetParam searchWidgetQueryParam
  case searchInput of
    Nothing -> return ()
    Just input ->
      case parseHumanReleaseSearch input of
        Left _ -> return ()
        Right rqir' ->
          redirect (SearchReleasesR, renderQueryReleaseSearch rqir')
  -- Here we process query specified is GET params.
  pparams <- lookupPagination
  rqir    <- lookupSearchParams
  paginated <- runDB (S.releaseQuery pparams (toReleaseFilters rqir) [])
  selectRep $ do
    -- HTML representation
    provideRep . noHeaderLayout $ do
      setTitle "Поиск публикаций"
      $(widgetFile "search-releases")
    -- JSON representation
    provideRep $ do
      render <- getUrlRender
      fmap paginatedJson . forM paginated $ \release -> do
        let (Entity rid r@Release {..}) = release
        stars <- runDB (S.starCount rid)
        u     <- fromJust <$> runDB (get releaseArtist)
        return (releaseJson render stars u r)

-- | Parse search parameters from GET parameters of current request.

lookupSearchParams :: Handler Rqir
lookupSearchParams = parseQueryReleaseSearch . reqGetParams <$> getRequest
