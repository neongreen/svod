-- |
-- Module      :  Handler.Releases
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Public information about releases (HTML and JSON).

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Releases
  ( getReleasesR )
where

import Helper.Access (userViaSlug)
import Import
import Widget.Release (releaseW)
import qualified Svod as S

-- | Get list of releases of a particular user.

getReleasesR :: Slug -> Handler TypedContent
getReleasesR slug = userViaSlug slug $ \user -> do
  let User {..} = entityVal user
      uid       = entityKey user
  render <- getUrlRender
  releases  <- runDB (S.getReleasesOfUser uid)
  selectRep $ do
    -- HTML representation
    provideRep . defaultLayout $ do
      setTitle (toHtml userName >> toHtml (" — дискография" :: Text))
      $(widgetFile "releases")
    -- JSON representation
    provideRep . return . toJSON . flip fmap (entityVal <$> releases) $
      \Release {..} -> object
        [ "title"       .= releaseTitle
        , "slug"        .= releaseSlug
        , "genre"       .= releaseGenre
        , "year"        .= releaseYear
        , "applied"     .= renderISO8601 releaseApplied
        , "description" .= releaseDesc
        , "license"     .= releaseLicense
        , "size"        .= releaseSize
        , "downloads"   .= releaseDownloads
        , "finalized"   .= (renderISO8601 <$> releaseFinalized)
        , "index"       .= unCatalogueIndex releaseIndex
        , "url"         .= render (ReleaseR userSlug releaseSlug) ]
