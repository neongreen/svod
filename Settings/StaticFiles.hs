-- |
-- Module      :  Settings.StaticFiles
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This generates easy references to files in the static directory at
-- compile time, providing compile-time verification that referenced files
-- exist. Warning: any files added to your static directory during run-time
-- can't be accessed this way.
--
-- For example, to refer to @static/js/script.js@ via an identifier, you'd
-- use:
--
-- > js_script_js
--
-- If the identifier is not available, you may use:
--
-- > StaticRoute ["js", "script.js"] []

{-# LANGUAGE TemplateHaskell #-}

module Settings.StaticFiles where

import Path (fromRelDir)
import Settings (appStaticDir, compileTimeAppSettings)
import Yesod.Static (staticFiles)

staticFiles (fromRelDir . appStaticDir $ compileTimeAppSettings)
