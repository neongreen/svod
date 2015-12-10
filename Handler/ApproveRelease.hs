-- |
-- Module      :  Handler.ApproveRelease
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Approve submitted release.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.ApproveRelease
  ( postApproveReleaseR )
where

import Import

-- | Approve submitted release. Only admins can do that.

postApproveReleaseR :: Handler Html
postApproveReleaseR = undefined
