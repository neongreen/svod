-- |
-- Module      :  Handler.Error
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Customized pages for various error codes and situations.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Error
  ( svodErrorHandler )
where

import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Import.NoFoundation
import Network.Wai (rawPathInfo)
import Yesod.Core (defaultErrorHandler)

-- | Collection of customized error pages. It's possible that we will
-- override more of these things in the future.

svodErrorHandler :: Yesod site
  => ErrorResponse
  -> HandlerT site IO TypedContent

svodErrorHandler NotFound = selectRep $ do
  provideRep . defaultLayout $ do
    request <- decodeUtf8With lenientDecode . rawPathInfo <$> waiRequest
    setTitle "404 Не найдено"
    $(widgetFile "404")
  provideRep . return $ object
    ["message" .= ("Not Found" :: Text)]

svodErrorHandler (PermissionDenied message) = selectRep $ do
  provideRep . defaultLayout $ do
    setTitle "403 Доступ запрещен"
    $(widgetFile "403")
  provideRep . return $ object
    ["message" .= ("Permission Denied. " <> message)]

svodErrorHandler (InternalError e) = selectRep $ do
  provideRep . defaultLayout $ do
    setTitle "500 Внутренняя ошибка сервера"
    $(widgetFile "500")
  provideRep . return $ object
    [ "message" .= ("Internal Server Error" :: Text)
    , "error"   .= e ]

svodErrorHandler response = defaultErrorHandler response
