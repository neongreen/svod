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
import Import
import qualified Svod as S

-- | Email verification. Note that this code relies on the fact that
-- authorization system won't let non-logged in users here. See "Foundation"
-- module.

getVerifyR
  :: Text              -- ^ Verification key
  -> Handler Html
getVerifyR verkey = do
  uid <- fromJust <$> maybeAuthId
  key <- fromJust <$> runDB (S.getVerifyKey uid)
  verified <- runDB (S.isVerified uid)
  when (not verified && verkey == key) $ do
    runDB (S.setVerified True uid)
    setMsg MsgSuccess "Регистрация подтверждена!"
  redirect HomeR
