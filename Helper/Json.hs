-- |
-- Module      :  Helper.Json
-- Copyright   :  © 2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module helps to generate JSON representation of objects we work with
-- in this application. The objects typically don't have 'toJSON' and
-- 'fromJSON' instances, because generation of the representations in often
-- involves messing with routes and additional info fetched from database
-- (e.g. number of stars).

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Helper.Json
  ( userJson
  , releaseJson
  , trackJson
  , notificationJson
  , paginatedJson )
where

import Helper.Rendering (toInt)
import Import
import Numeric.Natural
import qualified Svod as S

-- | Generate complete JSON representation of 'User'.

userJson
  :: (Route App -> Text) -- ^ Route render
  -> Natural           -- ^ Number of followers
  -> User              -- ^ The main bulk of data
  -> Value             -- ^ JSON value
userJson render followers User {..} = object
  -- TODO AVATARS Add location of avatar.
  [ "name"             .= userName
  , "slug"             .= userSlug
  , "email"            .= bool Nothing (Just userEmail) userEmailPublic
  , "website"          .= userWebsite
  , "description"      .= userDescription
  , "joined"           .= userJoined
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
  , "stars_url"        .= render (UserStarsR     userSlug)
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
  -- TODO COVERS Add location of covers.
  [ "artist"       .= userName
  , "title"        .= releaseTitle
  , "slug"         .= releaseSlug
  , "genre"        .= releaseGenre
  , "year"         .= toInt releaseYear
  , "applied"      .= releaseApplied
  , "description"  .= releaseDescription
  , "license"      .= licensePretty releaseLicense
  , "size"         .= toInt releaseSize
  , "downloads"    .= toInt releaseDownloads
  , "finalized"    .= releaseFinalized
  , "demo"         .= releaseDemo
  , "index"        .= unCatalogueIndex releaseIndex
  , "stars"        .= stars
  , "url"          .= render (ReleaseR userSlug releaseSlug)
  , "license_url"  .= licenseUrl releaseLicense
  , "artist_url"   .= render (UserR userSlug)
  , "archive_url"  .= render (ReleaseArchiveR  userSlug releaseSlug)
  , "tracks_url"   .= render (ReleaseTracksR   userSlug releaseSlug)
  , "approved_url" .= render (ReleaseApprovedR userSlug releaseSlug)
  , "starrers_url" .= render (ReleaseStarrersR userSlug releaseSlug) ]

-- | Generate JSON representation of track.

trackJson
  :: (Route App -> Text) -- ^ Route render
  -> User              -- ^ User, author of release
  -> Release           -- ^ Parent release of track
  -> Track             -- ^ Track description
  -> Value
trackJson render User {..} Release {..} track@Track {..} = object
  [ "number"        .= toInt trackNumber
  , "title"         .= trackTitle
  , "slug"          .= trackSlug
  , "description"   .= trackDescription
  , "duration"      .= unDuration trackDuration
  , "sample_rate"   .= toInt trackSampleRate
  , "bit_rate"      .= toInt trackBitRate
  , "sample_format" .= trackSampleRate
  , "channels"      .= toInt trackChannels
  , "url"           .= render (ReleaseTrackR userSlug releaseSlug trackSlug)
  , "artist_url"    .= render (UserR userSlug)
  , "release_url"   .= render (ReleaseR userSlug releaseSlug) ]
  where trackSlug = S.getTrackSlug track

-- | Generate complete JSON representation of 'Notification'.

notificationJson
  :: (Route App -> Text) -- ^ Route render
  -> Entity Notification -- ^ Actual 'Notification' object
  -> Value
notificationJson render (Entity nid Notification {..}) = object
  [ "type"        .= show notificationType
  , "mark_as_seen_url" .= render (NotificationSeenR nid)
  , "artist_url"  .= (render . UserR <$> muserSlug)
  , "release_url" .= (render <$> (ReleaseR <$> muserSlug <*> mreleaseSlug))
  , "description" .= notificationDescription
  , "date"        .= notificationDate ]
  where
    muserSlug, mreleaseSlug :: Maybe Slug
    muserSlug = mkSlug notificationArtist
    mreleaseSlug = mkSlug notificationRelease

-- | Represent paginated result as JSON value.

paginatedJson :: S.Paginated Value -> Value
paginatedJson p = object
  [ "items"       .= S.paginatedItems      p
  , "total_items" .= S.paginatedItemsTotal p
  , "total_pages" .= S.paginatedPagesTotal p
  , "page_size"   .= S.paginatedPageSize   p
  , "page"        .= S.paginatedPageIndex  p ]
