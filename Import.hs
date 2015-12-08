-- |
-- Module      :  Import
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This replaces "Prelude".

module Import
  ( module I )
where

import Foundation          as I
import Import.Message      as I
import Import.NoFoundation as I
