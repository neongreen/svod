-- |
-- Module      :  Handler.NotificationSeen
-- Copyright   :  © 2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Processing of AJAX events when user marks a notification as “seen”.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.NotificationSeen
  ( putNotificationSeenR )
where

import Data.Maybe (fromJust)
import Helper.Auth
import Import
import qualified Svod as S

-- | Process AJAX event when user marks a notification as “seen”.

putNotificationSeenR :: NotificationId -> Handler TypedContent
putNotificationSeenR nid = do
  checkCsrfParamNamed defaultCsrfParamName
  checkAuthWith isUser
  uid <- fromJust <$> maybeAuthId
  runDB (S.markAsSeen nid uid)
  return (toTypedContent ())
