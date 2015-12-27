-- |
-- Module      :  Widget.DngButton
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Configurable widget providing “dangerous buttons” — buttons that submit
-- forms (via POST) where all fields (specified by the user) are hidden. The
-- widget also asks to confirm every action initiated with it, because these
-- actions often have serious repercussions.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widget.DngButton
  ( BtnType (..)
  , dngButtonW )
where

import Data.Maybe (fromJust)
import Helper.Rendering (toJSONId)
import Import

-- | This controls appearance of buttons generated by 'dngButtonW' widget.

data BtnType
  = BtnDefault
  | BtnPrimary
  | BtnSuccess
  | BtnInfo
  | BtnWarning
  | BtnDanger
  | BtnLink

-- | Generate button to execute a dangerous action.

dngButtonW
  :: BtnType           -- ^ Control appearance of generated button
  -> Text              -- ^ Button title
  -> [(Text, Text)]    -- ^ POST parameters
  -> Route App         -- ^ Route
  -> Widget            -- ^ Button widget
dngButtonW btnType title params route = do
  csrfToken <- fromJust . reqToken <$> getRequest
  formId    <- newIdent
  $(widgetFile "dng-button")

-- | For internal usage only. Convert 'BtnType' to name of corresponding
-- Bootstrap 3 class.

btnTypeToClass :: BtnType -> Text
btnTypeToClass BtnDefault = "btn-default"
btnTypeToClass BtnPrimary = "btn-primary"
btnTypeToClass BtnSuccess = "btn-success"
btnTypeToClass BtnInfo    = "btn-info"
btnTypeToClass BtnWarning = "btn-warning"
btnTypeToClass BtnDanger  = "btn-danger"
btnTypeToClass BtnLink    = "btn-link"
