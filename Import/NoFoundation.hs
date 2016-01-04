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

module Import.NoFoundation
  ( module I
  , ζ
  , φ )
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
