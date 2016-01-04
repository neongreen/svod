-- |
-- Module      :  Main
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
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
