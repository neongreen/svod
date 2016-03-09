-- |
-- Module      :  Handler.Robots
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Serve the @robots.txt@ file.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Robots
  ( getRobotsR )
where

import Data.FileEmbed (embedFile)
import Import

-- | Serve the @robots.txt@ file.

getRobotsR :: Handler TypedContent
getRobotsR = return . TypedContent typePlain . toContent $
  $(embedFile "config/robots.txt")
