-- |
-- Module      :  Handler.Common
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Common
  ( getFaviconR
  , getRobotsR )
where

import Data.FileEmbed (embedFile)
import Import

-- | Favicon handler.

getFaviconR :: Handler TypedContent
getFaviconR = return . TypedContent "image/x-icon" . toContent $
  $(embedFile "config/favicon.ico")

-- | This one serves @robots.txt@ file.

getRobotsR :: Handler TypedContent
getRobotsR = return . TypedContent typePlain . toContent $
  $(embedFile "config/robots.txt")
