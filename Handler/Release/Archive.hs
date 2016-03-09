-- |
-- Module      :  Handler.Release.Archive
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This handler serves zip archives of published works for verified users
-- and also staging works for staff and authors.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Release.Archive
  ( getReleaseArchiveR )
where

import Helper.Access (releaseViaSlug)
import Helper.Path (getFConfig)
import Import
import Path
import Svod.LTS (FileLocation (..))
import qualified Svod as S

-- | Serve specified release.

getReleaseArchiveR
  :: Slug              -- ^ Artist slug
  -> Slug              -- ^ Release slug
  -> Handler TypedContent
getReleaseArchiveR uslug rslug = releaseViaSlug uslug rslug $ \_ release -> do
  let rid          = entityKey release
      Release {..} = entityVal release
      isFinalized  = isJust releaseFinalized
      contentType  = "application/zip"
  ownerHere <- ynAuth <$> isSelf uslug
  staffHere <- ynAuth <$> isStaff
  unless (isFinalized || ownerHere || staffHere) $
    permissionDenied "Эта работа ещё не опубликована."
  fconfig <- getFConfig
  outcome <- runDB (S.downloadRelease fconfig rid)
  case outcome of
    Left msg -> do
      setMsg MsgDanger (toHtml msg)
      redirect (ReleaseR uslug rslug)
    Right location -> case location of
      LocalFile path -> do
        addHeader "Content-Disposition" $
          "attachment; filename=" <> unSlug rslug <> ".zip"
        sendFile contentType (fromAbsFile path)
      CloudFile url -> redirect url
