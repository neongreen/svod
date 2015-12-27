-- |
-- Module      :  Handler.DownloadRelease
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This handler serves tarball contents for published works for normal
-- users, and also unpublished works for admins and authors.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.DownloadRelease
  ( getDownloadReleaseR )
where

import Import
import qualified Svod as S

-- | TODO Serve specified release.

getDownloadReleaseR :: Slug -> Slug -> Handler TypedContent
getDownloadReleaseR = undefined
