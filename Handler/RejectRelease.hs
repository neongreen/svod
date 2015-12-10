-- |
-- Module      :  Handler.RejectRelease
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Reject submitted release.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.RejectRelease
  ( postRejectReleaseR )
where

import Import

-- | Reject submitted release. Only admins and staff can do that.

postRejectReleaseR :: Handler Html
postRejectReleaseR = undefined
