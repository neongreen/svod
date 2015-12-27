-- |
-- Module      :  Handler.Notifications
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Page with notifications.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Notifications
  ( getNotificationsR )
where

import Import

-- | Serve page with notifications for currently logged-in user.

getNotificationsR :: Handler Html
getNotificationsR = undefined
