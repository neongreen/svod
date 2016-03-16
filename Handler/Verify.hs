-- |
-- Module      :  Handler.Verify
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Email verification.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Verify
  ( getVerifyR )
where

import Data.Maybe (fromJust)
import Helper.Auth
import Import
import qualified Svod as S

-- | Email verification.

getVerifyR
  :: Text              -- ^ Verification key
  -> Handler Html
getVerifyR verkey = do
  checkAuthWith isUser
  uid <- fromJust <$> maybeAuthId
  key <- fromJust <$> runDB (S.getVerifyKey uid)
  verified <- runDB (S.isVerified uid)
  when (not verified && verkey == key) $ do
    runDB (S.setVerified True uid)
    setMsg MsgSuccess "Регистрация подтверждена!"
  redirect HomeR
