-- |
-- Module      :  Handler.DownloadRelease
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This handler serves tarball contents for published works for normal
-- users, and also unpublished works for admins and authors.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.DownloadRelease
  ( getDownloadReleaseR )
where

import Helper.Access (releaseViaSlug)
import Helper.Path (getStagingDir, getReleaseDir)
import Import
import Path
import qualified Blaze.ByteString.Builder.ByteString as B
import qualified Data.ByteString as B
import qualified Svod as S

-- | Serve specified release.

getDownloadReleaseR
  :: Slug              -- ^ Artist slug
  -> Slug              -- ^ Release slug
  -> Handler TypedContent
getDownloadReleaseR uslug rslug = releaseViaSlug uslug rslug $ \_ release -> do
  let rid          = entityKey release
      Release {..} = entityVal release
      isFinalized  = isJust releaseFinalized
      contentType  = "application/x-tar"
  ownerHere <- ynAuth <$> isSelf uslug
  staffHere <- ynAuth <$> isStaff
  unless (isFinalized || ownerHere || staffHere) $
    permissionDenied "Эта работа ещё не опубликована."
  sroot <- getStagingDir
  rroot <- getReleaseDir
  outcome <- runDB (S.downloadRelease sroot rroot rid)
  case outcome of
    Left msg -> do
      setMsg MsgDanger (toHtml msg)
      redirect (ReleaseR uslug rslug)
    Right v -> do
      addHeader "Content-Disposition" $
        "attachment; filename=" <> unSlug rslug <> ".tar.gz"
      case v of
        Left content ->
          -- We have content of archive as strict byte string because no
          -- archive is stored in file system right now (the release is
          -- probably not yet finalized). This is not very efficient way to
          -- do things, but sometimes we need to resort to this (preview of
          -- publications waiting evaluation).
          sendResponse $ TypedContent contentType $ ContentBuilder
            (B.fromByteString content)
            (Just $ B.length content)
        Right path ->
          -- We have path to archive, so we can just tell backend to send
          -- it. This is more efficient for sure.
          sendFile contentType (fromAbsFile path)
