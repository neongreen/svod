-- |
-- Module      :  Handler.Home
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Home page handler.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Home
  ( getHomeR )
where

import Import

-- | Serve home page.

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Свод"
  $(widgetFile "homepage")
