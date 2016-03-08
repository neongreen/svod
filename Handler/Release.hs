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
  ( getReleaseR )
where

import Helper.Access (releaseViaSlug)
import Helper.Rendering (toInt, renderDescription)
import Import
import Widget.DngButton (BtnType (..), dngButtonW)
import Widget.StarRelease (starReleaseW)
import Widget.DownloadRelease (downloadReleaseW)
import qualified Svod as S

-- | Get public information about particular release in HTML or as JSON.

getReleaseR
  :: Slug              -- ^ User slug
  -> Slug              -- ^ Release slug
  -> Handler TypedContent
getReleaseR uslug rslug = releaseViaSlug uslug rslug $ \user release -> do
  timeZone  <- fmap (userTimeZone . entityVal) <$> maybeAuth
  ownerHere <- ynAuth <$> isSelf uslug
  staffHere <- ynAuth <$> isStaff
  adminHere <- ynAuth <$> isAdmin
  let User    {..} = entityVal user
      Release {..} = entityVal release
      isFinalized  = isJust releaseFinalized
      hasStatus    = not isFinalized || releaseDemo
  unless (isFinalized || ownerHere || staffHere) $
    permissionDenied "Эта работа ещё не опубликована."
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
      , "release-url"  .= render (ReleaseR uslug rslug)
      , "artist-url"   .= render (UserR uslug)
      , "genre"        .= releaseGenre
      , "year"         .= toInt releaseYear
      , "applied"      .= renderISO8601 releaseApplied
      , "desc"         .= releaseDesc
      , "license"      .= licensePretty releaseLicense
      , "license-url"  .= licenseUrl releaseLicense
      , "size"         .= toInt releaseSize
      , "downloads"    .= toInt releaseDownloads
      , "download-url" .= render (DownloadReleaseR uslug rslug)
      , "finalized"    .= (renderISO8601 <$> releaseFinalized)
      , "demo"         .= releaseDemo
      , "index"        .= unCatalogueIndex releaseIndex ]
