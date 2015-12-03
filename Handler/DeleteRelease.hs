-- |
-- Module      :  Handler.DeleteRelease
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Delete any release, even already published.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.DeleteRelease
  ( postDeleteReleaseR )
where

import Import

-- | Delete any release, even already published. Only admins can do
-- that. Just like deletion of users, this should be used with care, only
-- when you absolutely sure that you have to delete it (for example, for
-- legal reasons).

postDeleteReleaseR :: Handler Html
postDeleteReleaseR = undefined
