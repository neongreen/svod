-- |
-- Module      :  Widget.FollowUser
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- A widget to follow users.
--
-- See also: "Handler.FollowUser".

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widget.FollowUser
  ( followUserW )
where

import Helper.Access (userViaSlug')
import Import
import qualified Data.Text.Encoding as TE
import qualified Svod as S

-- | This widget adds a button that allows users to follow other
-- users. Actual click is processed via AJAX, see corresponding Julius
-- template.

followUserW :: Slug -> Widget
followUserW slug = userViaSlug' slug $ \target' -> do
  let φ = handlerToWidget . runDB
      target = entityKey target'
  muid      <- handlerToWidget maybeAuthId
  buttonId  <- newIdent
  counterId <- newIdent
  iconId    <- newIdent
  following <- case muid of
    Nothing  -> return False
    Just uid -> φ (S.isFollower target uid)
  count'    <- φ (S.followerCount target)
  let count         = fromIntegral count'           :: Int
      activeTitle   = "Не следить за пользователем" :: Text
      inactiveTitle = "Следить за пользователем"    :: Text
      activeIcon    = "glyphicon-eye-close"         :: Text
      inactiveIcon  = "glyphicon-eye-open"          :: Text
  addScript (StaticR js_cookie_js)
  if isJust muid
  then $(widgetFile "follow-user-logged-in")
  else $(widgetFile "follow-user-guest")
  $(widgetFile "follow-user")
