-- |
-- Module      :  Handler.ChangePassword
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The procedure of password changing.

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.ChangePassword
  ( getChangePasswordR
  , postChangePasswordR )
where

import Import
import Yesod.Form.Bootstrap3
import qualified Svod as S

-- | Information user need to provide in order to change his\/her password.

data ChangePasswordForm = ChangePasswordForm
  { cpOldPass  :: Text -- ^ Old password
  , cpNewPass0 :: Text -- ^ New password
  , cpNewPass1 :: Text -- ^ New password repeated
  }

-- | Serve page with form allowing to change user' password.

getChangePasswordR :: Handler Html
getChangePasswordR = undefined

-- | Process results of password change form.

postChangePasswordR :: Handler Html
postChangePasswordR = undefined
