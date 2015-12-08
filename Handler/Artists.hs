-- |
-- Module      :  Handler.Artists
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Display paginated list of users-artists or return public info about them
-- in JSON format.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Artists
  ( getArtistsR )
where

import Import

-- | Serve paginated list of users-artists or return list of them in JSON
-- format.

getArtistsR :: Handler TypedContent
getArtistsR = undefined
