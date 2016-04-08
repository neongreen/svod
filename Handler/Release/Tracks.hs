-- |
-- Module      :  Handler.Release.Tracks
-- Copyright   :  © 2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Serve tracklist of a release.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Release.Tracks
  ( getReleaseTracksR )
where

import Helper.Access (releaseViaSlug)
import Helper.Auth
import Helper.Json (trackJson)
import Import
import qualified Data.List.NonEmpty as NE
import qualified Svod               as S

-- | Serve tracklist of a release.

getReleaseTracksR :: Slug -> Slug -> Handler TypedContent
getReleaseTracksR uslug rslug = releaseViaSlug uslug rslug $ \user release -> do
  let (Entity _   u) = user
      (Entity rid r) = release
      isFinalized    = isJust (releaseFinalized r)
  unless isFinalized $
    checkAuthWith (isSelf uslug <> isStaff)
  selectRep . provideRep $ do
    render <- getUrlRender
    tracks <- runDB (S.getReleaseTracklist rid)
    return . toJSON . NE.toList . fmap (trackJson render u r) $ tracks
