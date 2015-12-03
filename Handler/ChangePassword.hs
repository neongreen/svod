-- |
-- Module      :  Handler.ChangePassword
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The procedure of password changing.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.ChangePassword
  ( getChangePasswordR
  , postChangePasswordR )
where

import Import

-- | Serve page with form allowing to change user' password.

getChangePasswordR :: Handler Html
getChangePasswordR = undefined

-- | Process results of password change form.

postChangePasswordR :: Handler Html
postChangePasswordR = undefined
