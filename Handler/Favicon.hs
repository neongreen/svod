-- |
-- Module      :  Handler.Favicon
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Serve favicon of the site.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Favicon
  ( getFaviconR )
where

import Data.FileEmbed (embedFile)
import Import

-- | Serve favicon of the site.

getFaviconR :: Handler TypedContent
getFaviconR = return . TypedContent "image/x-icon" . toContent $
  $(embedFile "config/favicon.ico")
