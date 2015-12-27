-- |
-- Module      :  Import.NoFoundation
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Single import file including all basic things, kind of custom
-- "Prelude".

module Import.NoFoundation
  ( module I
  , toJSONId
  , toInt )
where

import ClassyPrelude.Yesod   as I hiding
  ((</>), setMessage, ReleaseType (..), count)
import Settings              as I
import Settings.StaticFiles  as I
import Svod.Model            as I
import Yesod.Auth            as I hiding (LoginR, LogoutR)
import Yesod.Core.Types      as I (loggerSet)
import Yesod.Default.Config2 as I

-- | A helper to render identifiers in jQuery as JSON objects.

toJSONId :: Text -> Value
toJSONId text = toJSON ("#" <> text)

-- | Convert more exotic stuff like 'Natural' into plain 'Integer' suitable
-- for direct interpolation into templates.

toInt :: Integral a => a -> Integer
toInt = fromIntegral
