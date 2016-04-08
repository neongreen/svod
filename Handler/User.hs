-- |
-- Module      :  Handler.User
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Get information about particular user.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.User
  ( getUserR
  , deleteUserR
  , patchUserR )
where

import Handler.User.Profile (processUserChange)
import Helper.Access (userViaSlug)
import Helper.Auth
import Helper.Json (userJson)
import Helper.Path (getFConfig)
import Helper.Property (changeUserProperty)
import Helper.Rendering (renderDescription)
import Import
import Widget.DngButton (BtnType (..), dngButtonW)
import Widget.FollowUser (followUserW)
import Widget.User (userStatusLabelsW)
import qualified Svod as S

-- | Get information about particular user in HTML or JSON.

getUserR :: Slug -> Handler TypedContent
getUserR slug = userViaSlug slug $ \user -> do
  timeZoneOffset <- fmap (userTimeZoneOffset . entityVal) <$> maybeAuth
  ownerHere <- ynAuth <$> isSelf slug
  staffHere <- ynAuth <$> isStaff
  adminHere <- ynAuth <$> isAdmin
  let u@User {..} = entityVal user
      userAdmin   = userStatus == AdminUser
      userStaff   = userStatus `elem` [StaffUser, AdminUser]
      uid         = entityKey user
  userAuthor <- not . null <$> runDB (S.getReleasesOfUser uid)
  let hasStatus = or
        [ userAdmin
        , userStaff
        , userAuthor
        , not userVerified
        , userBanned && staffHere ]
  selectRep $ do
    -- TODO AVATARS Show real avatars instead of placeholders.
    -- HTML representation
    provideRep . noHeaderLayout $ do
      setTitle (toHtml userName)
      $(widgetFile "user")
    -- JSON representation
    provideRep $ do
      render    <- getUrlRender
      followers <- runDB (S.followerCount uid)
      return (userJson render followers u)

-- | Delete specified user.

deleteUserR :: Slug -> Handler TypedContent
deleteUserR slug = do
  checkAuthWith isAdmin
  fc <- getFConfig
  changeUserProperty (S.deleteUser fc) (const UsersR) slug

-- | Partially edit user profile.

patchUserR :: Slug -> Handler Html
patchUserR = processUserChange
