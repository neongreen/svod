-- |
-- Module      :  Handler.Release.Data
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Display and edit parameters of submitted release. Admins can edit even
-- already published works.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Release.Data
  ( getReleaseDataR
  , postReleaseDataR )
where

import Import

-- | Render form for release editing.

getReleaseDataR
  :: Slug              -- ^ User slug
  -> Slug              -- ^ Release slug
  -> Handler Html
getReleaseDataR = undefined -- TODO

-- | Process release editing request.

postReleaseDataR
  :: Slug              -- ^ User slug
  -> Slug              -- ^ Release slug
  -> Handler Html
postReleaseDataR = undefined -- TODO
