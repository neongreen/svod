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
  , deleteReleaseR
  , putReleaseR )
where

import Handler.Release.Edit (processReleaseChange)
import Helper.Access (releaseViaSlug)
import Helper.Auth
import Helper.Json (releaseJson)
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
  timeZoneOffset <- fmap (userTimeZoneOffset . entityVal) <$> maybeAuth
  ownerHere <- ynAuth <$> isSelf uslug
  adminHere <- ynAuth <$> isAdmin
  let u@User    {..} = entityVal user
      r@Release {..} = entityVal release
      rid            = entityKey release
      isFinalized    = isJust releaseFinalized
      hasStatus      = not isFinalized || releaseDemo
  unless isFinalized $
    checkAuthWith (isSelf uslug <> isStaff)
  tracks <- runDB (S.getReleaseTracklist rid)
  let totalDur = totalDuration (trackDuration <$> tracks)
  selectRep $ do
    -- HTML representation
    provideRep . noHeaderLayout $ do
      setTitle (toHtml releaseTitle)
      adminTextAreaId <- newIdent
      $(widgetFile "release")
    -- JSON representation
    provideRep $ do
      render <- getUrlRender
      stars  <- runDB (S.starCount rid)
      return (releaseJson render stars u r)

-- | Delete specified release.

deleteReleaseR :: Slug -> Slug -> Handler TypedContent
deleteReleaseR uslug rslug = do
  checkAuthWith isAdmin
  changeReleaseProperty S.deleteRelease (\_ _ -> PendingReleasesR) uslug rslug

-- | Edit release (this mostly just replaces existing release).

putReleaseR :: Slug -> Slug -> Handler TypedContent
putReleaseR = processReleaseChange
