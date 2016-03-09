-- |
-- Module      :  Handler.Releases
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Public information about releases (HTML and JSON).

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Releases
  ( getReleasesR )
where

import Import

-- | Get list of releases either in paginated form (HTML), or as JSON.

getReleasesR :: Slug -> Handler TypedContent
getReleasesR = undefined

      -- stars <- mapM (runDB . S.starCount . entityKey) releases
      -- let f (s, e) = let x = entityVal e in object
      --       [ "title" .= releaseTitle x
      --       , "url"   .= render (ReleaseR userSlug (releaseSlug x))
      --       , "year"  .= toInt (releaseYear x)
      --       , "stars" .= toInt s ]
-- (f <$> zip stars releases)
