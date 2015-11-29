-- |
-- Module      :  Main
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Development version of main module.

module Main (main) where

import Application (appMain)
import Prelude     (IO)

main :: IO ()
main = appMain
