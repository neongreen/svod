-- |
-- Module      :  Import.NoFoundation
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Single import file including all basic things, kind of custom "Prelude".

module Import.NoFoundation
  ( module I )
where

import ClassyPrelude.Yesod   as I
import Settings              as I
import Settings.StaticFiles  as I
import Svod.Model            as I
import Yesod.Auth            as I
import Yesod.Core.Types      as I (loggerSet)
import Yesod.Default.Config2 as I
