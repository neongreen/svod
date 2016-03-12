-- |
-- Module      :  Helper.Json
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module helps to generate JSON representation of objects we work with
-- in this application. The objects typically don't have 'toJSON' and
-- 'fromJSON' instances, because generation of the representations in often
-- should be more flexible.

{-# LANGUAGE NoImplicitPrelude #-}

module Helper.Json
  ( userJson
  , releaseJson )
where

import Data.Bool (bool)
import Helper.Rendering (toInt)
import Import
import Numeric.Natural

-- | Generate complete JSON representation of 'User'.

userJson
  :: (Route App -> Text) -- ^ Route render
  -> Natural           -- ^ Number of followers
  -> User              -- ^ The main bulk of data
  -> Value             -- ^ JSON value
userJson render followers User {..} = object
  [ "name"             .= userName
  , "slug"             .= userSlug
  , "email"            .= bool Nothing (Just userEmail) userEmailPublic
  , "website"          .= userWebsite
  , "description"      .= userDescription
  , "joined"           .= renderISO8601 userJoined
  , "time_zone_offset" .= userTimeZoneOffset
  , "admin"            .= userAdmin
  , "staff"            .= userStaff
  , "banned"           .= userBanned
  , "followers"        .= followers
  , "verified"         .= userVerified
  , "url"              .= render (UserR userSlug)
  , "profile_url"      .= render (UserProfileR   userSlug)
  , "verified_url"     .= render (UserVerifiedR  userSlug)
  , "banned_url"       .= render (UserBannedR    userSlug)
  , "staff_url"        .= render (UserStaffR     userSlug)
  , "admin_url"        .= render (UserAdminR     userSlug)
  , "followers_url"    .= render (UserFollowersR userSlug)
  , "releases_url"     .= render (ReleasesR      userSlug) ]
  where userAdmin = userStatus == AdminUser
        userStaff = userAdmin || userStatus == StaffUser

-- | Generate complete JSON representation of 'Release'.

releaseJson
  :: (Route App -> Text) -- ^ Route render
  -> Natural           -- ^ Number of stars
  -> User              -- ^ Information about author (artist)
  -> Release           -- ^ The main bulk of data
  -> Value
releaseJson render stars User {..} Release {..} = object
  [ "artist"       .= userName
  , "title"        .= releaseTitle
  , "slug"         .= releaseSlug
  , "genre"        .= releaseGenre
  , "year"         .= toInt releaseYear
  , "applied"      .= renderISO8601 releaseApplied
  , "description"  .= releaseDescription
  , "license"      .= licensePretty releaseLicense
  , "size"         .= toInt releaseSize
  , "downloads"    .= toInt releaseDownloads
  , "finalized"    .= (renderISO8601 <$> releaseFinalized)
  , "demo"         .= releaseDemo
  , "index"        .= unCatalogueIndex releaseIndex
  , "stars"        .= stars
  , "url"          .= render (ReleaseR userSlug releaseSlug)
  , "license_url"  .= licenseUrl releaseLicense
  , "artist_url"   .= render (UserR userSlug)
  , "archive_url"  .= render (ReleaseArchiveR  userSlug releaseSlug)
  , "data_url"     .= render (ReleaseDataR     userSlug releaseSlug)
  , "approved_url" .= render (ReleaseApprovedR userSlug releaseSlug)
  , "starrers_url" .= render (ReleaseStarrersR userSlug releaseSlug) ]
