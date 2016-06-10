-- |
-- Module      :  Import.NoFoundation
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Single import file including all basic things, kind of custom
-- "Prelude".

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Import.NoFoundation
  ( module I
  , ζ
  , φ
  , cdnJQuery
  , cdnBootstrap
  , cdnAnchorJs
  , cdnCookieJs )
where

import ClassyPrelude.Yesod   as I hiding
  ((</>), setMessage, ReleaseType (..), count)
import Settings              as I
import Settings.StaticFiles  as I
import Svod.Model            as I
import Yesod.Auth            as I hiding (LoginR, LogoutR)
import Yesod.Core.Types      as I (loggerSet)
import Yesod.Default.Config2 as I

-- | A synonym for 'handlerToWidget'.

ζ :: Monad m => HandlerT site m a -> WidgetT site m a
ζ = handlerToWidget

-- | Lift database actions to widget level.

φ :: YesodPersist site => YesodDB site a -> WidgetT site IO a
φ = handlerToWidget . runDB

-- | Add remote jQuery copy (from CDN) to current widget.

cdnJQuery :: MonadWidget m => m ()
cdnJQuery = addScriptRemoteAttrs url
  [("integrity", hsh), ("crossorigin", origin)]
  where
    url = "https://code.jquery.com/jquery-2.2.3.min.js"
    hsh = "sha256-a23g1Nt4dtEYOj7bR+vTu7+T8VP13humZFBJNIYoEJo="
    origin = "anonymous"

-- | Add remote Bootstrap 3 copy (from CDN) to current widget.

cdnBootstrap :: MonadWidget m => m ()
cdnBootstrap = do
  addStylesheetRemoteAttrs cssUrl
    [("integrity", cssHash), ("crossorigin", origin)]
  addScriptRemoteAttrs jsUrl
    [("integrity", jsHash), ("crossorigin", origin)]
  where
    cssUrl = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
    cssHash = "sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7"
    jsUrl = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
    jsHash = "sha384-0mSbJDEHialfmuBBQP6A4Qrprq5OVfW37PRR3j5ELqxss1yVqOtnepnHVP9aJ7xS"
    origin = "anonymous"

-- | Add remote Anchor JS copy (from CDN) to current widget.

cdnAnchorJs :: MonadWidget m => m ()
cdnAnchorJs = addScriptRemote
  "https://cdnjs.cloudflare.com/ajax/libs/anchor-js/3.1.0/anchor.min.js"

-- | Add remote JS Chookie copy (from CDN) to current widget.

cdnCookieJs :: MonadWidget m => m ()
cdnCookieJs = addScriptRemote
  "https://cdnjs.cloudflare.com/ajax/libs/js-cookie/2.1.0/js.cookie.min.js"
