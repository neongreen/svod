-- |
-- Module      :  Widget.StarRelease
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- A widget to star releases.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widget.StarRelease
  ( starReleaseW )
where

import Helper.Access (releaseViaSlug')
import Import
import qualified Data.Text.Encoding as TE
import qualified Svod as S

-- | This widget adds a button that allows to star releases. Actual click is
-- processed via AJAX, see corresponding Julius template.

starReleaseW
  :: Slug              -- ^ Artist slug
  -> Slug              -- ^ Release slug
  -> Widget            -- ^ Resulting widget
starReleaseW aslug rslug = releaseViaSlug aslug rslug $ \artist' release' -> do
  let φ = handlerToWidget . runDB
      release = entityKey release'
  muid      <- handlerToWidget maybeAuthId
  buttonId  <- newIdent
  counterId <- newIdent
  iconId    <- newIdent
  let lgogedIn = isJust muid
  starred <- case muid of
    Nothing  -> return False
    Just uid -> φ (S.isStarredBy release uid)
  count'  <- φ (S.starCount release)
  let count         = fromIntegral count'      :: Int
      activeTitle   = "Не так уж это и хорошо" :: Text
      inactiveTitle = "Отметить публикацию"    :: Text
      activeIcon    = "glyphicon-star"         :: Text
      inactiveIcon  = "glyphicon-star-empty"   :: Text
  addScript (StaticR js_cookie_js)
  $(widgetFile "star-release")
