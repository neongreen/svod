-- |
-- Module      :  Import.Message
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Replacement for the original 'setMessage' mechanism, slightly boosted
-- variation of it.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Import.Message
  ( MsgType (..)
  , setMsg )
where

import ClassyPrelude.Yesod
import Text.Hamlet (shamletFile)

-- | Type of message. At the end of the day it specifies which class to
-- assign to rendered message.

data MsgType
  = MsgSuccess         -- ^ For success messages
  | MsgInfo            -- ^ “For your information” messages
  | MsgWarning         -- ^ For warnings
  | MsgDanger          -- ^ Error and the like
    deriving (Eq, Show, Enum, Bounded)

-- | Boosted 'setMessage'. This one takes 'MsgType' and 'Html' contents to
-- show in message (usually generated with something like 'shamlet').

setMsg :: MonadHandler m
  => MsgType           -- ^ Message type
  -> Html              -- ^ Format string
  -> m ()
setMsg msgType message =
  setMessage $(shamletFile "templates/message.hamlet")

-- | For internal usage. Convert 'MsgType' to name of corresponding
-- Bootstrap 3 class.

msgTypeToClass :: MsgType -> Text
msgTypeToClass MsgSuccess = "alert-success"
msgTypeToClass MsgInfo    = "alert-info"
msgTypeToClass MsgWarning = "alert-warning"
msgTypeToClass MsgDanger  = "alert-danger"
