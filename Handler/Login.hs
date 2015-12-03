-- |
-- Module      :  Handler.Login
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Login handler.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Login
  ( getLoginR
  , postLoginR )
where

import Import

-- | Serve a page with login form.

getLoginR :: Handler Html
getLoginR = undefined

-- | Process submitted login form.

postLoginR :: Handler Html
postLoginR = undefined
