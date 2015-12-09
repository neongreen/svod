-- |
-- Module      :  Handler.EditProfile
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Edit user profile.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.EditProfile
  ( getEditProfileR
  , postEditProfileR )
where

import Import

-- | Serve page containing form that allows to edit user profile.

getEditProfileR :: Text -> Handler Html
getEditProfileR slug = undefined

-- | Process submitted form and refresh user's profile.

postEditProfileR :: Text -> Handler Html
postEditProfileR slug = undefined
