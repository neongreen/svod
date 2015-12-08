-- |
-- Module      :  Import.Message
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Replacement for the original 'setMessage' mechanism,slightly boosted
-- variation of it.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Import.Message
  ( MsgType (..)
  , setMsg )
where

import ClassyPrelude.Yesod
import Text.Hamlet (shamletFile)
import qualified Data.Text.Lazy as TL
import qualified Text.Markdown  as MD

-- | Type of message. At the end of the day it affects which class to assign
-- to rendered message.

data MsgType
  = MsgSuccess         -- ^ For success messages
  | MsgInfo            -- ^ “For your information” messages
  | MsgWarning         -- ^ For warnings
  | MsgDanger          -- ^ Error and the like
    deriving (Eq, Show, Enum, Bounded)

-- | Boosted 'setMessage'. This one takes 'MsgType', then format string as
-- for 'sformat' function, then collection of arguments. Its contents is
-- plain markdown.

setMsg :: MonadHandler m
  => MsgType           -- ^ Message type
  -> Text              -- ^ Format string
  -> m ()
setMsg msgType raw =
  let message   = MD.markdown MD.def (TL.fromStrict raw)
      isSuccess = msgType == MsgSuccess
      isInfo    = msgType == MsgInfo
      isWarning = msgType == MsgWarning
      isDanger  = msgType == MsgDanger
  in setMessage $(shamletFile "templates/message.hamlet")
