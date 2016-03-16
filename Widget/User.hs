-- |
-- Module      :  Widget.User
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Widget displaying info about given user compactly.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widget.User
  ( userW
  , userStatusLabelsW )
where

import Helper.Auth
import Import
import Widget.FollowUser (followUserW)
import qualified Svod as S

-- | Display most important information about given user.

userW :: Entity User -> Widget
userW user = do
  let User {..}   = entityVal user
      placeholder = StaticR $ StaticRoute ["img", "user", "ph_60.jpg"] []
  $(widgetFile "user-widget")

-- | Generate collection of labels describing a user.

userStatusLabelsW
  :: Entity User       -- ^ User in question
  -> Maybe Bool        -- ^ Do you already know if he has releases?
  -> Widget
userStatusLabelsW user isAuthor = do
  let User {..}   = entityVal user
      uid         = entityKey user
      userAdmin   = userStatus == AdminUser
      userStaff   = userStatus `elem` [StaffUser, AdminUser]
  userAuthor <- case isAuthor of
    Nothing -> not . null <$> φ (S.getReleasesOfUser uid)
    Just b  -> return b
  staffHere <- ynAuth <$> ζ isStaff
  $(widgetFile "user-status-labels")
