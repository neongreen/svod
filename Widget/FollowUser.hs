-- |
-- Module      :  Widget.FollowUser
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
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

import Import
import qualified Data.Text.Encoding as TE
import qualified Svod as S

-- | This widget adds a button that allows users to follow other
-- users. Actual click is processed via AJAX, see corresponding Julius
-- template.

followUserW :: Slug -> Widget
followUserW slug = do
  let φ = handlerToWidget . runDB
  mtarget <- φ (S.getUserBySlug slug)
  case entityKey <$> mtarget of
    Nothing -> notFound
    Just target -> do
      muid <- handlerToWidget maybeAuthId
      buttonId  <- newIdent
      counterId <- newIdent
      iconId    <- newIdent
      let loggedIn = isJust muid
      following <- case muid of
        Nothing  -> return False
        Just uid -> φ $ S.isFollower target uid
      count'    <- φ (S.followerCount target)
      let count         = fromIntegral count'           :: Int
          activeTitle   = "Не следить за пользователем" :: Text
          inactiveTitle = "Следить за пользователем"    :: Text
          activeIcon    = "glyphicon-eye-close"         :: Text
          inactiveIcon  = "glyphicon-eye-open"          :: Text
      addScript (StaticR js_cookie_js)
      $(widgetFile "follow-user")
