-- |
-- Module      :  Handler.Release.Starrers
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Serve list of users who have starred particular release (determined by
-- URL).

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Release.Starrers
  ( getReleaseStarrersR )
where

import Import

getReleaseStarrersR :: Slug -> Slug -> Handler Html
getReleaseStarrersR = undefined -- TODO
