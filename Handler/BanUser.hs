-- |
-- Module      :  Handler.BanUser
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Ban user. Admins and staff in general can ban users at will, here is
-- where it happens.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.BanUser
  ( postBanUserR )
where

import Import

-- | Process the request to ban some user.

postBanUserR :: Handler Html
postBanUserR = undefined
