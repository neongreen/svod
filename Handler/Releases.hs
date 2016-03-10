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
  ( getReleasesR
  , postReleasesR )
where

import Handler.SubmitRelease (processReleaseSubmission)
import Helper.Access (userViaSlug)
import Helper.Json (releaseJson)
import Import
import Widget.Release (releaseW)
import qualified Svod as S

-- | Get list of releases of a particular user.

getReleasesR :: Slug -> Handler TypedContent
getReleasesR slug = userViaSlug slug $ \user -> do
  let u@User {..} = entityVal user
      uid         = entityKey user
  releases  <- runDB (S.getReleasesOfUser uid)
  selectRep $ do
    -- HTML representation
    provideRep . defaultLayout $ do
      setTitle (toHtml userName >> toHtml (" — дискография" :: Text))
      $(widgetFile "releases")
    -- JSON representation
    provideRep $ do
      render <- getUrlRender
      return . toJSON $ releaseJson render Nothing u
        <$> (entityVal <$> releases)

-- | Process submission of a new release.

postReleasesR :: Slug -> Handler TypedContent
postReleasesR = processReleaseSubmission
