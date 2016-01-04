-- |
-- Module      :  Handler.EditRelease
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Edit submitted release. Admins can edit even already published ones.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.EditRelease
  ( getEditReleaseR
  , postEditReleaseR )
where

import Import

-- | Render form for release editing.

getEditReleaseR
  :: Slug              -- ^ User slug
  -> Slug              -- ^ Release slug
  -> Handler Html
getEditReleaseR = undefined

-- | Process release editing request.

postEditReleaseR
  :: Slug              -- ^ User slug
  -> Slug              -- ^ Release slug
  -> Handler Html
postEditReleaseR = undefined
