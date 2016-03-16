-- |
-- Module      :  Widget.Pagination
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Pagination widget and helpers.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widget.Pagination
  ( lookupPagination
  , paginationW )
where

import Data.Foldable (notElem)
import Data.Maybe (fromJust)
import Helper.Rendering (toInt)
import Import hiding (notElem)
import Numeric.Natural
import qualified Data.Text      as T
import qualified Data.Text.Read as TR
import qualified Svod           as S

-- | Parse pagination parameters from GET parameters of current
-- request. Default page number is 1 and default page size is 15.

lookupPagination :: Handler S.Pagination
lookupPagination = do
  let readWithDefault n = maybe n (either (const n) fst . TR.decimal)
      defaultPageSize   = 15
      defaultPageNum    = 1
      defaultPagination =
        fromJust (S.mkPagination defaultPageSize defaultPageNum)
  pageSize <- readWithDefault defaultPageSize <$> lookupGetParam pageSizeParam
  pageNum  <- readWithDefault defaultPageNum <$> lookupGetParam pageNumParam
  return $ fromMaybe defaultPagination (S.mkPagination pageSize pageNum)

-- | Display pagination widget.

paginationW :: Route App -> S.Paginated a -> Widget
paginationW route p@S.Paginated {..} = do
  params <- reqGetParams <$> φ getRequest
  let reach         = 2
      noPrev        = not (S.paginatedHasPrev p)
      noNext        = not (S.paginatedHasNext p)
      pageRange     = S.paginatedPageRange p reach
      firstSpec     = 1 `notElem` pageRange
      lastSpec      = paginatedPages `notElem` pageRange
      backwardEllip = S.paginatedBackwardEllip p reach
      forwardEllip  = S.paginatedForwardEllip p reach
      toPage        = mkPageLink route params
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
pageSizeParam = "pagesize"

-- | Name of “page number” query string parameter.

pageNumParam :: Text
pageNumParam = "page"
