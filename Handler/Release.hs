-- |
-- Module      :  Handler.Release
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
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
import Import
import Widget.DngButton (BtnType (..), dngButtonW)
import Widget.StarRelease (starReleaseW)
import qualified Svod          as S
import qualified Text.Markdown as MD

-- | Get public information about particular release in HTML or as JSON.

getReleaseR
  :: Text              -- ^ User slug
  -> Text              -- ^ Release slug
  -> Handler TypedContent
getReleaseR uslug' rslug' =
  let uslug = mkSlug uslug'
      rslug = mkSlug rslug'
  in releaseViaSlug uslug rslug $ \user release -> do
    verifiedHere <- ynAuth <$> isVerified
    ownerHere    <- ynAuth <$> isSelf uslug'
    staffHere    <- ynAuth <$> isStaff
    adminHere    <- ynAuth <$> isAdmin
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
        let desc = MD.markdown MD.def (fromStrict releaseDesc)
            s    = [("user-slug", uslug'), ("release-slug", rslug')]
            approveBtn = dngButtonW BtnSuccess "Одобрить" s ApproveReleaseR
            rejectBtn  = dngButtonW BtnWarning "Отвергнуть" s RejectReleaseR
            deleteBtn  = dngButtonW BtnDanger "Удалить" s DeleteReleaseR
        $(widgetFile "release")
      -- JSON representation
      provideRep . return . object $
        maybeToList (("genre" .=) <$> releaseGenre) ++
        maybeToList (("finalized" .=) . toInt <$> releaseFinalized) ++
        [ "artist"      .= userName
        , "title"       .= releaseTitle
        , "release-url" .= render (ReleaseR uslug' rslug')
        , "artist-url"  .= render (UserR uslug')
        , "year"        .= toInt releaseYear
        , "applied"     .= timePretty releaseApplied
        , "desc"        .= releaseDesc
        , "license"     .= licensePretty releaseLicense
        , "license-url" .= licenseUrl releaseLicense
        , "downloads"   .= toInt releaseDownloads
        , "demo"        .= releaseDemo
        , "index"       .= unCatalogueIndex releaseIndex ]
