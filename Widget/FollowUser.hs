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
  ( followUserW )
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
  following <- case entityKey <$> muser of
    Nothing  -> return False
    Just uid -> φ (S.isFollower target uid)
  count'    <- φ (S.followerCount target)
  let count = fromIntegral count' :: Int
  cdnCookieJs
  case entityVal <$> muser of
    Nothing -> $(widgetFile "follow-user-widget-guest")
    Just User {..} ->
      if userVerified
        then $(widgetFile "follow-user-widget-logged-in")
        else $(widgetFile "follow-user-widget-unverified")
  $(widgetFile "follow-user-widget")
