-- |
-- Module      :  Widget.Search
-- Copyright   :  © 2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Search bar widget.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widget.Search
  ( searchW
  , searchWidgetQueryParam )
where

import Import

-- | Search widget. This is a fake placeholder for now.

searchW
  :: Route App         -- ^ The route to help article
  -> Route App         -- ^ “Action” route
  -> Text              -- ^ Default import
  -> Widget            -- ^ The search widget
searchW helpRoute actionRoute input = $(widgetFile "search-widget")

-- | Name of parameters that will contain the raw search query.

searchWidgetQueryParam :: Text
searchWidgetQueryParam = "ввод"
