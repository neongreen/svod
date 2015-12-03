-- |
-- Module      :  Handler.Register
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The registration form.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Register
  ( getRegisterR
  , postRegisterR )
where

import Import

-- | Serve registration page.

getRegisterR :: Handler Html
getRegisterR = undefined

-- | Process registration request.

postRegisterR :: Handler Html
postRegisterR = undefined
