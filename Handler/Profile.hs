-- |
-- Module      :  Handler.Profile
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- View and edit user profile.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Profile
  ( getProfileR
  , postProfileR )
where

import Import

-- | Serve page containing form that allows to edit user profile.

getProfileR :: Handler Html
getProfileR = undefined

-- | Process submitted form and refresh user's profile.

postProfileR :: Handler Html
postProfileR = undefined
