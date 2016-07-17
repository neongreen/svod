-- |
-- Module      :  Widget.Pagination
-- Copyright   :  © 2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Pagination widget and helpers.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widget.Pagination
  ( lookupPagination
  , paginationW )
where

import Data.Foldable (notElem)
import Data.Maybe (fromJust)
import Data.Pagination
import Helper.Rendering (toInt)
import Import hiding (notElem)
import Numeric.Natural
import qualified Data.Text      as T
import qualified Data.Text.Read as TR

-- | Parse pagination parameters from GET parameters of current
-- request. Default page number is 1 and default page size is 15.

lookupPagination :: Handler Pagination
lookupPagination = do
  let readWithDefault n = maybe n (either (const n) fst . TR.decimal)
      defaultPageSize   = 15
      defaultPageNum    = 1
      defaultPagination =
        fromJust (mkPagination defaultPageSize defaultPageNum)
  psize   <- readWithDefault defaultPageSize <$> lookupGetParam pageSizeParam
  pindex  <- readWithDefault defaultPageNum <$> lookupGetParam pageNumParam
  return $ fromMaybe defaultPagination (mkPagination psize pindex)

-- | Display pagination widget.

paginationW
  :: Route App         -- ^ Route of page where the pagination is displayed
  -> Paginated a       -- ^ The paginated data
  -> Widget            -- ^ The pagination widget
paginationW route p = do
  params <- reqGetParams <$> φ getRequest
  let reach         = 2
      noPrev        = not (hasPrevPage p)
      noNext        = not (hasNextPage p)
      prange        = pageRange p reach
      firstSpec     = 1 `notElem` prange
      lastSpec      = paginatedPagesTotal p `notElem` prange
      bellip        = backwardEllip p reach
      fellip        = forwardEllip p reach
      toPage        = mkPageLink route params
      pindex        = pageIndex (paginatedPagination p)
  $(widgetFile "pagination-widget")

-- | Create route preserving given query string parameters but with
-- specified page.

mkPageLink
  :: Route App         -- ^ Current route
  -> [(Text, Text)]    -- ^ Collection of query string parameters
  -> Natural           -- ^ Page number
  -> (Route App, [(Text, Text)]) -- ^ Resulting value ready for interpolation
mkPageLink route params  0  = (route, params)
mkPageLink route params' n' = (route, (pageNumParam, n) : params)
  where n = T.pack (show n')
        params = filter ((/= pageNumParam) . fst) params'

-- | Name of “page size” query string parameter.

pageSizeParam :: Text
pageSizeParam = "на_стр"

-- | Name of “page number” query string parameter.

pageNumParam :: Text
pageNumParam = "стр"
