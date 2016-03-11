-- |
-- Module      :  Helper.Form
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Helpers for constructing Bootstrap- and API-friendly forms.

{-# LANGUAGE NoImplicitPrelude #-}

module Helper.Form
  ( μ
  , μ'
  , μL )
where

import Formatting (sformat, int)
import Import
import Yesod.Form.Bootstrap3

-- | Generate Bootstrap-friendly 'FieldSettings' given name of field on form
-- (important for API) and label.

μ :: Text -> Text -> FieldSettings App
μ name label = FieldSettings
  { fsLabel   = SomeMessage label
  , fsTooltip = Nothing
  , fsId      = Nothing
  , fsName    = Just name
  , fsAttrs   = [("class", "form-control")] }

-- | The same as 'μ', but make the field get focus automatically.

μ' :: Text -> Text -> FieldSettings App
μ' name label = withAutofocus (μ name label)

-- | Generate field with minimum and maximum constraints.

μL :: Text -> Text -> Int -> Int -> FieldSettings App
μL name label miny maxy = x
  { fsAttrs =
      ("min", sformat int miny) :
      ("max", sformat int maxy) :
      fsAttrs x }
  where x = μ name label
