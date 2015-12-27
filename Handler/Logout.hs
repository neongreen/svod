-- |
-- Module      :  Handler.Logout
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Process logout request.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Logout
  ( getLogoutR )
where

import Import

-- | Logout user.

getLogoutR :: Handler TypedContent
getLogoutR = toTypedContent <$> clearCreds True
