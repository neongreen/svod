-- |
-- Module      :  Widget.Search
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Search bar widget.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widget.Search
  ( searchW )
where

import Import

-- | Search widget. This is a fake placeholder for now.

searchW :: Widget
searchW = $(widgetFile "search-widget")
