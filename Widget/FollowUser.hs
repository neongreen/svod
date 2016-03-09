-- |
-- Module      :  Widget.FollowUser
-- Copyright   :  © 2015–2016 Mark Karpov
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
  ( followUserW
  , isFollowedBy
  , followedIcon )
where

import Helper.Access (userViaSlug')
import Helper.Rendering (toJSONId)
import Import
import qualified Data.Text.Encoding as TE
import qualified Svod as S

-- | This widget adds a button that allows users to follow other
-- users. Actual click is processed via AJAX, see corresponding Julius
-- template.

followUserW :: Slug -> Widget
followUserW tslug = userViaSlug' tslug $ \target' -> do
  let target = entityKey target'
  muser     <- ζ maybeAuth
  buttonId  <- newIdent
  counterId <- newIdent
  iconId    <- newIdent
  following <- isFollowedBy target (entityKey <$> muser)
  count'    <- φ (S.followerCount target)
  let count         = fromIntegral count'           :: Int
      inactiveTitle = "Следить за пользователем"    :: Text
      activeTitle   = "Не следить за пользователем" :: Text
      inactiveIcon  = followedIcon False
      activeIcon    = followedIcon True
  addScript (StaticR js_cookie_js)
  case entityVal <$> muser of
    Nothing -> $(widgetFile "follow-user-guest")
    Just User {..} ->
      if userVerified
        then $(widgetFile "follow-user-logged-in")
        else $(widgetFile "follow-user-unverified")
  $(widgetFile "follow-user")

-- | Check if particular user is follower by given user.

isFollowedBy
  :: UserId            -- ^ Whom to follow
  -> Maybe UserId      -- ^ Potential follower
  -> WidgetT App IO Bool -- ^ Does this user follows the first one?
isFollowedBy target muid =
  case muid of
    Nothing -> return False
    Just uid -> φ (S.isFollower target uid)

-- | Return name of class to use for “number of followers” icon depending on
-- whether it's followed by actual logged-in user or not.

followedIcon :: Bool -> Text
followedIcon False = "glyphicon-eye-open"
followedIcon True  = "glyphicon-eye-close"
