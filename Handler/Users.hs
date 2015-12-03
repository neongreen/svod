-- |
-- Module      :  Handler.Users
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Display paginated list of users or return public info about them in JSON
-- format.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Users
  ( getUsersR )
where

import Import

-- | Serve paginated list of users or return list of them in JSON format.

getUsersR :: Handler TypedContent
getUsersR = undefined
