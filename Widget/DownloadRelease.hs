-- |
-- Module      :  Widget.DownloadRelease
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This widget implements “download release” button.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widget.DownloadRelease
  ( downloadReleaseW )
where

import Helper.Rendering (toJSONId)
import Import

-- | Button that allows to download releases.

downloadReleaseW
  :: Slug              -- ^ Artist slug
  -> Slug              -- ^ Release slug
  -> Widget            -- ^ Resulting widget
downloadReleaseW uslug rslug = do
  muser    <- ζ maybeAuth
  buttonId <- newIdent
  case entityVal <$> muser of
    Nothing -> $(widgetFile "download-release-guest")
    Just User {..} ->
      unless userVerified
        $(widgetFile "download-release-unverified")
  $(widgetFile "download-release")
