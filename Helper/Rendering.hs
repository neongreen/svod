-- |
-- Module      :  Helper.Rendering
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Helpers for value interpolation in templates when data has a bit
-- non-standard type.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper.Rendering
  ( toJSONId
  , toInt
  , renderDescription )
where

import CMark (CMarkOption)
import Import
import Text.Blaze.Html (preEscapedToHtml)
import qualified CMark as C

-- | A helper to render identifiers in jQuery as JSON objects.

toJSONId :: Text -> Value
toJSONId text = toJSON ("#" <> text)

-- | Convert more exotic stuff like 'Natural' into plain 'Integer' suitable
-- for direct interpolation in templates.

toInt :: Integral a => a -> Integer
toInt = fromIntegral

-- | Render description containing markdown to HTML.

renderDescription :: Description -> Html
renderDescription = preEscapedToHtml . C.commonmarkToHtml cmarkOpts . unDescription

-- | CMark options that are used to control markdown rendering.

cmarkOpts :: [CMarkOption]
cmarkOpts = [C.optNormalize, C.optSmart, C.optSafe]
