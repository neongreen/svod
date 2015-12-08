-- |
-- Module      :  Handler.Artist
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Get information about particular user-artist, in HTML or JSON.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Artist
  ( getArtistR )
where

import Import

-- | Get information about particular user-artist in HTML or JSON.

getArtistR :: Text -> Handler TypedContent
getArtistR = undefined
