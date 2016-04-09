-- |
-- Module      :  Widget.Notification
-- Copyright   :  © 2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The notification widget.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widget.Notification
  ( notificationW )
where

import Helper.Rendering (renderDescription, toJSONId)
import Import
import qualified Data.Text.Encoding as TE

-- | The compact notification widget.

notificationW
  :: Bool              -- ^ Already seen notification?
  -> Entity Notification -- ^ The notification itself
  -> Widget
notificationW seen (Entity nid Notification {..}) = do
  timeZoneOffset <- fmap (userTimeZoneOffset . entityVal) <$> ζ maybeAuth
  panelId  <- newIdent
  buttonId <- newIdent
  let unseen   = not seen
      artistR  = fromMaybe HomeR
        (UserR <$> mkSlug notificationArtist)
      releaseR = fromMaybe HomeR
        (ReleaseR <$> mkSlug notificationArtist <*> mkSlug notificationRelease)
  cdnCookieJs
  $(widgetFile "notification-widget")
