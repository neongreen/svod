-- |
-- Module      :  Handler.Release
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Get public information about particular release (HTML or JSON).

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Release
  ( getReleaseR
  , deleteReleaseR )
where

import Helper.Access (releaseViaSlug)
import Helper.Auth (checkAuthWith)
import Helper.Property (changeReleaseProperty)
import Helper.Rendering (toInt, renderDescription)
import Import
import Widget.DngButton (BtnType (..), dngButtonW)
import Widget.DownloadRelease (downloadReleaseW)
import Widget.StarRelease (starReleaseW)
import qualified Svod as S

-- | Get public information about particular release in HTML or as JSON.

getReleaseR
  :: Slug              -- ^ User slug
  -> Slug              -- ^ Release slug
  -> Handler TypedContent
getReleaseR uslug rslug = releaseViaSlug uslug rslug $ \user release -> do
  timeZone  <- fmap (userTimeZone . entityVal) <$> maybeAuth
  ownerHere <- ynAuth <$> isSelf uslug
  adminHere <- ynAuth <$> isAdmin
  let User    {..} = entityVal user
      Release {..} = entityVal release
      isFinalized  = isJust releaseFinalized
      hasStatus    = not isFinalized || releaseDemo
  unless isFinalized $
    checkAuthWith (isSelf uslug <> isStaff)
  render <- getUrlRender
  tracks <- runDB . S.getReleaseTracklist . entityKey $ release
  let totalDur = totalDuration (trackDuration <$> tracks)
      placeholder = StaticR $ StaticRoute ["img", "user", "placeholder.jpg"] []
  -- ↑ FIXME In the future we will need to render real cover here.
  selectRep $ do
    -- HTML representation
    provideRep . defaultLayout $ do
      setTitle (toHtml releaseTitle)
      $(widgetFile "release")
    -- JSON representation
    provideRep . return . object $
      [ "artist"       .= userName
      , "title"        .= releaseTitle
      , "slug"         .= releaseSlug
      , "genre"        .= releaseGenre
      , "year"         .= toInt releaseYear
      , "applied"      .= renderISO8601 releaseApplied
      , "description"  .= releaseDesc
      , "license"      .= licensePretty releaseLicense
      , "size"         .= toInt releaseSize
      , "downloads"    .= toInt releaseDownloads
      , "finalized"    .= (renderISO8601 <$> releaseFinalized)
      , "demo"         .= releaseDemo
      , "index"        .= unCatalogueIndex releaseIndex
      , "license_url"  .= licenseUrl releaseLicense
      , "release_url"  .= render (ReleaseR uslug rslug)
      , "artist_url"   .= render (UserR uslug)
      , "archive_url"  .= render (ReleaseArchiveR uslug rslug)
      , "data_url"     .= render (ReleaseDataR uslug rslug)
      , "approved_url" .= render (ReleaseApprovedR uslug rslug)
      , "starrers_url" .= render (ReleaseStarrersR uslug rslug) ]

-- | Delete specified release.

deleteReleaseR :: Slug -> Slug -> Handler TypedContent
deleteReleaseR uslug rslug = do
  checkAuthWith isAdmin
  changeReleaseProperty S.deleteRelease (\s _ -> ReleasesR s) uslug rslug
