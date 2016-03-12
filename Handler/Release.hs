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
      placeholder = StaticR $ StaticRoute ["img", "release", "ph_230.jpg"] []
  -- ↑ FIXME In the future we will need to render real cover here.
  selectRep $ do
    -- HTML representation
    provideRep . defaultLayout $ do
      setTitle (toHtml releaseTitle)
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
  changeReleaseProperty S.deleteRelease (\s _ -> ReleasesR s) uslug rslug
