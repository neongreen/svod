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

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Helper.Form
  ( μ
  , μ'
  , μL
  , licenseField
  , getCurrentYear
  , yearField
  , tracksField )
where

import Data.List.NonEmpty (NonEmpty)
import Formatting (sformat, int)
import Helper.Rendering (toInt, toJSONId)
import Import
import Yesod.Form.Bootstrap3
import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T
import qualified Svod               as S

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

-- | A drop-down field containing all supported license options.

licenseField :: Field Handler License
licenseField = selectFieldList licenses
  where licenses = (licensePretty &&& id) <$> [minBound..maxBound]

-- | Get current year.

getCurrentYear :: IO Int
getCurrentYear = do
  (year, _, _) <- toGregorian . utctDay <$> getCurrentTime
  return (fromIntegral year)

-- | Generate year field given lower and upper bounds for this value.

yearField
  :: Int               -- ^ Lower bound for year (inclusive)
  -> Int               -- ^ Upper bound for year (inclusive)
  -> Field Handler Int
yearField miny maxy = check checkYear intField
  where
    checkYear :: Int -> Either Text Int
    checkYear year
      | year < miny = Left "Слишком давно."
      | year > maxy = Left "Машина времени?"
      | otherwise   = Right year

-- | Custom field to collect variable-length list of tracks, or rather
-- instructions how to create tracks in form of 'S.CreateTrack' data types.
--
-- Every field /must/ provide name of track and actual audio file in FLAC
-- format.

tracksField :: Field Handler (NonEmpty (Text, FileInfo))
tracksField = Field parse view Multipart
  where
    parse titles' files' =
      return . either (Left . SomeMessage) (Right . Just) $ do
        titles <- mapM checkTitle titles'
        files  <- mapM checkUploadedFile files'
        return . NE.fromList $ zip titles files
    view ident name attrs given required =
      let baseId      = ident  <> "-"
          addTrackId  = baseId <> "add-track"
          remTrackId  = baseId <> "rem-track"
          trackListId = ident
          nums        = sformat int <$> [1..S.maxTrackNumber]
          items       = zip nums $ case given of
            Left _ -> repeat Nothing
            Right xs -> NE.toList (Just <$> xs) ++ repeat Nothing
      in $(widgetFile "tracks-field")

-- | Check single title.

checkTitle :: Text -> Either Text Text
checkTitle title =
  -- TODO LINT Add additional checks here.
  if T.null title
    then Left "Название не может быть пустым."
    else Right title

-- | Check single uploaded file.

checkUploadedFile :: FileInfo -> Either Text FileInfo
checkUploadedFile info =
  if fileContentType info == "audio/flac"
    then Right info
    else Left "Неверный формат, только FLAC является приемлемым форматом."
