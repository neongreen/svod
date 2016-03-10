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
  , deleteUserR )
where

import Data.Bool (bool)
import Helper.Access (userViaSlug)
import Helper.Auth (checkAuthWith)
import Helper.Property (changeUserProperty)
import Helper.Path (getFConfig)
import Helper.Rendering (renderDescription)
import Import
import Widget.DngButton (BtnType (..), dngButtonW)
import Widget.FollowUser (followUserW)
import Widget.Release (releaseW)
import qualified Svod as S

-- | Get information about particular user in HTML or JSON.

getUserR :: Slug -> Handler TypedContent
getUserR slug = userViaSlug slug $ \user -> do
  timeZone  <- fmap (userTimeZone . entityVal) <$> maybeAuth
  ownerHere <- ynAuth <$> isSelf slug
  staffHere <- ynAuth <$> isStaff
  adminHere <- ynAuth <$> isAdmin
  let User {..} = entityVal user
      userAdmin = userStatus == AdminUser
      userStaff = userStatus `elem` [StaffUser, AdminUser]
      uid       = entityKey user
  releases  <- runDB (S.getReleasesOfUser uid)
  render    <- getUrlRender
  let userAuthor = not (null releases)
      hasStatus = or
        [ userAdmin
        , userStaff
        , userAuthor
        , not userVerified
        , userBanned && staffHere ]
      email = fromMaybe "-" (emailPretty userEmail)
      placeholder = StaticR $ StaticRoute ["img", "user", "placeholder.jpg"] []
  selectRep $ do
    -- HTML representation
    provideRep . defaultLayout $ do
      setTitle (toHtml userName)
      $(widgetFile "user")
    -- JSON representation
    provideRep . return . object $
      [ "name"             .= userName
      , "slug"             .= userSlug
      , "email"            .= bool Nothing (Just userEmail) userEmailPublic
      , "website"          .= userWebsite
      , "desc"             .= userDesc
      , "joined"           .= renderISO8601 userJoined
      , "time_zone_offset" .= userTimeZone
      , "admin"            .= userAdmin
      , "staff"            .= userStaff
      , "banned"           .= userBanned
      , "verified"         .= userVerified
      , "url"              .= render (UserR userSlug)
      , "profile_url"      .= render (UserProfileR   userSlug)
      , "verified_url"     .= render (UserVerifiedR  userSlug)
      , "banned_url"       .= render (UserBannedR    userSlug)
      , "staff_url"        .= render (UserStaffR     userSlug)
      , "admin_url"        .= render (UserAdminR     userSlug)
      , "followers_url"    .= render (UserFollowersR userSlug)
      , "releases_url"     .= render (ReleasesR      userSlug) ]

-- | Delete specified user.

deleteUserR :: Slug -> Handler TypedContent
deleteUserR slug = do
  checkAuthWith isAdmin
  fc <- getFConfig
  changeUserProperty (S.deleteUser fc) (const UsersR) slug
