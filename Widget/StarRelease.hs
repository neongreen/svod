-- |
-- Module      :  Widget.StarRelease
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- A widget to star releases.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widget.StarRelease
  ( starReleaseW
  , isStarredBy )
where

import Helper.Access (releaseViaSlug')
import Helper.Rendering (toJSONId)
import Import
import qualified Data.Text.Encoding as TE
import qualified Svod as S

-- | This widget adds a button that allows to star releases. Actual click is
-- processed via AJAX, see corresponding Julius template.

starReleaseW
  :: Slug              -- ^ Artist slug
  -> Slug              -- ^ Release slug
  -> Widget            -- ^ Resulting widget
starReleaseW aslug rslug = releaseViaSlug' aslug rslug $ \_ release' -> do
  let rid = entityKey release'
  muser     <- ζ maybeAuth
  buttonId  <- newIdent
  counterId <- newIdent
  starred   <- isStarredBy rid (entityKey <$> muser)
  count'    <- φ (S.starCount rid)
  let count = fromIntegral count' :: Int
  addScript (StaticR js_cookie_js)
  case entityVal <$> muser of
    Nothing -> $(widgetFile "star-release-guest")
    Just User {..} ->
      if userVerified
        then $(widgetFile "star-release-logged-in")
        else $(widgetFile "star-release-unverified")
  $(widgetFile "star-release")

-- | Check if particular release is starred by given user.

isStarredBy
  :: ReleaseId         -- ^ Release in question
  -> Maybe UserId      -- ^ Identity of logged in user (if any)
  -> WidgetT App IO Bool -- ^ Is this release starred by this user?
isStarredBy rid muid =
  case muid of
    Nothing -> return False
    Just uid -> φ (S.isStarredBy rid uid)
