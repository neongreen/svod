-- |
-- Module      :  Handler.User
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Get information about particular user, in HTML or JSON.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.User
  ( getUserR )
where

import Import

-- | Get information about particular user in HTML or JSON.

getUserR :: Text -> Handler TypedContent
getUserR = undefined
