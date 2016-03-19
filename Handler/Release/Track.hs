-- |
-- Module      :  Handler.Release.Track
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Per-track pages.

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Release.Track
  ( getReleaseTrackR
  , putReleaseTrackR )
where

import Helper.Access (releaseViaSlug)
import Helper.Auth
import Helper.Form
import Helper.Json (trackJson)
import Helper.Rendering (toInt, toJSONId, renderDescription)
import Import
import Yesod.Form.Bootstrap3
import qualified Data.List.NonEmpty as NE
import qualified Svod               as S

-- | Form for editing of track description.

data TrackDescriptionForm = TrackDescriptionForm
  { tdDescription :: Maybe Textarea -- ^ Description
  }

-- | Track description form.

trackDescriptionForm :: Maybe Text -> Form TrackDescriptionForm
trackDescriptionForm mdesc =
  renderBootstrap3 BootstrapBasicForm $ TrackDescriptionForm
    <$> aopt textareaField (μ "description" "Текст и описание")
      (Just $ Textarea <$> mdesc)

-- | Serve page about a particular track in release.

getReleaseTrackR :: Slug -> Slug -> Slug -> Handler TypedContent
getReleaseTrackR = serveReleaseTrack (generateFormPost . trackDescriptionForm)

-- | Replace description of track.

putReleaseTrackR :: Slug -> Slug -> Slug -> Handler TypedContent
putReleaseTrackR uslug rslug tslug =
  releaseViaSlug uslug rslug $ \_ release -> do
    checkAuthWith (isStaff <> isSelf uslug)
    let rid = entityKey release
    tracks <- runDB (S.getReleaseTracklist rid)
    case S.slugToTrack tracks tslug of
      Nothing -> notFound
      Just Track {..} -> do
        let mdesc = unDescription <$> trackDescription
        ((result, form), enctype) <- runFormPost (trackDescriptionForm mdesc)
        case result of
          FormSuccess TrackDescriptionForm {..} -> do
            let mdesc' = mkDescription . unTextarea <$> tdDescription
            runDB (S.setTrackDescription mdesc' rid trackNumber)
          _ -> return ()
        serveReleaseTrack (const $ return (form, enctype)) uslug rslug tslug

serveReleaseTrack :: ToWidget App a
  => (Maybe Text -> Handler (a, Enctype))
  -> Slug
  -> Slug
  -> Slug
  -> Handler TypedContent
serveReleaseTrack fe uslug rslug tslug =
  releaseViaSlug uslug rslug $ \user release -> do
    let (Entity _   u) = user
        (Entity rid r) = release
        isFinalized    = isJust (releaseFinalized r)
    unless isFinalized $
      checkAuthWith (isSelf uslug <> isStaff)
    ownerHere <- ynAuth <$> isSelf uslug
    staffHere <- ynAuth <$> isStaff
    tracks <- runDB (S.getReleaseTracklist rid)
    case S.slugToTrack tracks tslug of
      Nothing -> notFound
      Just track@Track {..} -> selectRep $ do
        -- HTML representation
        provideRep . defaultLayout $ do
          setTitle (toHtml trackTitle)
          (form, enctype) <- ζ $ fe (unDescription <$> trackDescription)
          togglerId  <- newIdent
          formId     <- newIdent
          let durations = S.trackDuration <$> tracks
              cost :: Double
              cost = unDuration (totalDuration durations) / 100
              getWidth :: Duration -> Int
              getWidth = floor . (/ cost) . unDuration
              trackOffset, posStart, posLength, posRest :: Int
              trackOffset = fromIntegral trackNumber - 1
              posStart  =
                getWidth . totalDuration $ NE.take trackOffset durations
              (posLength, posRest) =
                if fromIntegral trackNumber == NE.length durations
                  then (100 - posStart, 0)
                  else let x = getWidth trackDuration
                       in (x, 100 - x - posStart)
              firstSlug = S.getTrackSlug (NE.head tracks)
              prevSlug = S.getTrackSlug $
                if trackOffset == 0
                  then NE.last tracks
                  else tracks NE.!! pred trackOffset
              nextSlug = S.getTrackSlug $
                if trackOffset == NE.length tracks - 1
                  then NE.head tracks
                  else tracks NE.!! succ trackOffset
              lastSlug  = S.getTrackSlug (NE.last tracks)
          $(widgetFile "release-track")
        -- JSON representation
        provideRep $ do
          render <- getUrlRender
          return (trackJson render u r track)
