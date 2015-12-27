-- |
-- Module      :  Handler.ReleaseActions
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Administrative actions on releases.
--
-- TODO When we have a page with all pending releases, it's better to
-- redirect there after rejection or deletion instead of 'HomeR'.

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.ReleaseActions
  ( postApproveReleaseR
  , postRejectReleaseR
  , postDeleteReleaseR )
where

import Helper.Access (releaseViaSlug)
import Helper.Path (getStagingDir, getReleaseDir)
import Import
import Path
import qualified Svod as S

-- | Approve submitted release. Only admins can do that.
--
-- POST request should have @"user-slug"@ and @"release-slug"@ parameter
-- identifying release to approve and 'defaultCsrfParamName' parameter
-- containing CSRF-protection token.

postApproveReleaseR :: Handler TypedContent
postApproveReleaseR = postAdministrative S.approveRelease ReleaseR

-- | Reject submitted release. Only admins and staff can do that.
--
-- POST request should have @"user-slug"@ and @"release-slug"@ parameter
-- identifying release to reject and 'defaultCsrfParamName' parameter
-- containing CSRF-protection token.

postRejectReleaseR :: Handler TypedContent
postRejectReleaseR = postAdministrative rejectRelease (\_ _ -> HomeR)
  where rejectRelease sroot _ = S.rejectRelease sroot

-- | Delete any release, even already published. Only admins can do
-- that. Just like deletion of users, this should be used with care, only
-- when you absolutely sure that you have to delete it (for example, for
-- legal reasons).
--
-- POST request should have @"user-slug"@ and @"release-slug"@ parameter
-- identifying release to delete and 'defaultCsrfParamName' parameter
-- containing CSRF-protection token.

postDeleteReleaseR :: Handler TypedContent
postDeleteReleaseR = postAdministrative S.deleteRelease (\_ _ -> HomeR)

-- | Generalized version of administrative action on release. Release is
-- always identified by combination of @"user-slug"@ and @"release-slug"@
-- parameters of POST request.

postAdministrative
  :: (Path Abs Dir -> Path Abs Dir -> ReleaseId -> YesodDB App (Either Text a))
     -- ^ Action to perform
  -> (Slug -> Slug -> Route App) -- ^ Where to redirect given pair of slugs
  -> Handler TypedContent
postAdministrative action route = do
  checkCsrfParamNamed defaultCsrfParamName
  (uslug', rslug') <- runInputPost $ (,)
    <$> ireq textField "user-slug"
    <*> ireq textField "release-slug"
  uslug <- parseSlug uslug'
  rslug <- parseSlug rslug'
  releaseViaSlug uslug rslug $ \_ release -> do
    let rid = entityKey release
    sroot   <- getStagingDir
    rroot   <- getReleaseDir
    outcome <- runDB (action sroot rroot rid)
    case outcome of
      Left msg -> do
        setMsg MsgDanger (toHtml msg)
        redirect (ReleaseR uslug rslug)
      Right _ -> redirect (route uslug rslug)
