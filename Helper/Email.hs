-- |
-- Module      :  Helper.Email
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Helpers for email verification (and email sending as part of the
-- verification process).

{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Helper.Email
  ( startEmailVerificationCycle )
where

import Import
import Network.Mail.Mime hiding (htmlPart)
import System.IO.Unsafe (unsafePerformIO)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (shamletFile)
import Text.Shakespeare.Text (textFile)
import qualified Crypto.Nonce            as Nonce
import qualified Data.Text.Lazy.Builder  as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Svod                    as S

-- | Start email verification cycle. This sets status of specified user to
-- “unverified”, generates and sets verification key and sends email with
-- verification link to specified email address.
--
-- Note that during development, email is automatically gets verified for
-- simplicity.
--
-- See also: "Handler.Verify".

startEmailVerificationCycle
  :: Email             -- ^ Email address to verify
  -> Text              -- ^ User name (to mention in email)
  -> UserId            -- ^ Identifier of user
  -> Handler ()
startEmailVerificationCycle email name uid = do
#if DEVELOPMENT
  runDB (S.setVerified True uid)
  setMsg MsgSuccess "Профиль автоматически активирован."
#else
  runDB (S.setVerified False uid)
  verkey <- liftIO randomKey
  runDB (S.setVerifyKey verkey uid)
  render <- getUrlRender
  sendEmail name email (render $ VerifyR verkey)
  setMsg MsgInfo [shamlet|
Мы выслали вам ссылку для подтверждения регистрации на #
<strong>
  #{unEmail email}
.
|]
#endif

-- | Generate a random alphanumeric string.

randomKey :: IO Text
randomKey = Nonce.nonce128urlT defaultNonceGen

-- | Taken from "Yesod.Auth.Email".

defaultNonceGen :: Nonce.Generator
defaultNonceGen = unsafePerformIO Nonce.new
{-# NOINLINE defaultNonceGen #-}

-- | Send email with link that user has to click in order to verify
-- (activate) his\/her account.
--
-- This uses standard system utility @sendmail@. It's good enough for now.

sendEmail
  :: Text              -- ^ User name
  -> Email             -- ^ Email address
  -> Text              -- ^ Verification URL
  -> Handler ()
sendEmail name email url = do
  render <- getUrlRender
  let textPart = Part
        { partType     = "text/plain; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partContent  = T.encodeUtf8 . T.toLazyText $
           $(textFile "templates/verification-email.txt") render
        , partHeaders  = [] }
      htmlPart = Part
        { partType     = "text/html; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partContent  = renderHtml
           $(shamletFile "templates/verification-email.hamlet")
        , partHeaders  = [] }
  liftIO $ renderSendMail
    (emptyMail $ Address (Just "Проект «Свод»") "noreply")
    { mailTo      = [Address Nothing (unEmail email)]
    , mailHeaders = [("Subject", "Подтверждение регистрации")]
    , mailParts   = [[textPart, htmlPart]] }
