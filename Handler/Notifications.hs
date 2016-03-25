-- |
-- Module      :  Handler.Notifications
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Notification page.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Notifications
  ( getNotificationsR )
where

import Data.Maybe (fromJust)
import Helper.Auth
import Helper.Json (notificationJson, paginatedJson)
import Import
import Widget.Notification (notificationW)
import Widget.Pagination (lookupPagination, paginationW)
import qualified Svod as S

-- | Serve page with notifications for currently logged-in user.

getNotificationsR :: Handler TypedContent
getNotificationsR = do
  checkAuthWith isUser
  (Entity uid User {..}) <- fromJust <$> maybeAuth
  params    <- lookupPagination
  paginated <- runDB (S.getUserNotifications params uid)
  selectRep $ do
    -- HTML representation
    provideRep . noHeaderLayout $ do
      setTitle "Уведомления"
      $(widgetFile "notifications")
    provideRep $ do
      render <- getUrlRender
      let items = notificationJson render . snd <$> S.paginatedItems paginated
      return (paginatedJson paginated { S.paginatedItems = items })
