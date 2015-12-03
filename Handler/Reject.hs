-- |
-- Module      :  Handler.Reject
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Reject submitted release.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Reject
  ( postRejectR )
where

import Import

-- | Reject submitted release. Only admins and staff can do that.

postRejectR :: Handler Html
postRejectR = undefined
