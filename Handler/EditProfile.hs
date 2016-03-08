-- |
-- Module      :  Handler.EditProfile
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Edit user profile. Note that admins can edit profile of any user.

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.EditProfile
  ( getEditProfileR
  , postEditProfileR )
where

import Helper.Access (userViaSlug)
import Import
import Yesod.Form.Bootstrap3
import qualified Svod as S

-- | Editable pieces of user profile.

data EditProfileForm = EditProfileForm
  { epEmailPublic    :: Bool           -- ^ Whether user's email is public
  , epTimeZoneOffset :: TimeZoneOffset -- ^ User's time zone
  , epWebsite        :: Maybe Text     -- ^ Website
  , epDesc           :: Maybe Textarea -- ^ Description (“About me”)
  }

-- | Everything user can edit in his profile is on this form. The argument
-- of the function is used to populate default values for the form.

editProfileForm :: User -> Form EditProfileForm
editProfileForm User {..} =
  renderBootstrap3 BootstrapBasicForm $ EditProfileForm
    <$> areq checkBoxField
      (withAutofocus $ bfs
       ("Публичный адрес почты (" <> email <> ")" :: Text))
      (Just userEmailPublic)
    <*> areq (selectFieldList timeZones)
      (bfs ("Часовой пояс" :: Text)) (Just userTimeZone)
    <*> aopt urlField (bfs ("Ваш сайт" :: Text))
      (Just userWebsite)
    <*> aopt textareaField (bfs ("Расскажите о себе" :: Text))
      (Just . Textarea . unDescription <$> userDesc)
  where email = fromMaybe "<неверный формат>" (emailPretty userEmail)
        timeZones :: [(Text, TimeZoneOffset)]
        timeZones =
          [ ("Москва (+3)",                             TimeZoneOffset 180)
          , ("Самара (+4)",                             TimeZoneOffset 240)
          , ("Екатеринбург (+5)",                       TimeZoneOffset 300)
          , ("Омск, Новосибирск (+6)",                  TimeZoneOffset 360)
          , ("Красноярск, Норильск (+7)",               TimeZoneOffset 420)
          , ("Иркутск, Чита (+8)",                      TimeZoneOffset 480)
          , ("Якутск (+9)",                             TimeZoneOffset 540)
          , ("Комсомольск-на-Амуре, Магадан (+10)",     TimeZoneOffset 600)
          , ("Среднеколымск (+11)",                     TimeZoneOffset 660)
          , ("Петропавловск-камчатский, Анадырь (+12)", TimeZoneOffset 720) ]

-- | Serve page containing form that allows to edit user profile.

getEditProfileR :: Slug -> Handler Html
getEditProfileR slug = userViaSlug slug $ \user -> do
  (form, enctype) <- generateFormPost . editProfileForm . entityVal $ user
  serveEditProfile slug form enctype

-- | Process submitted form and refresh user's profile.

postEditProfileR :: Slug -> Handler Html
postEditProfileR slug = userViaSlug slug $ \user -> do
  ((result, form), enctype) <-
    runFormPost . editProfileForm . entityVal $ user
  case result of
    FormSuccess EditProfileForm {..} -> do
      runDB $ S.editUserProfile
        (entityKey user)
        epEmailPublic
        epTimeZoneOffset
        epWebsite
        (mkDescription . unTextarea <$> epDesc)
      render <- getUrlRender
      let profileUrl = render (UserR slug)
      setMsg MsgSuccess [shamlet|
Профиль пользователя #
<a href="#{profileUrl}">
  #{userName $ entityVal user}
\ обновлен успешно.
|]
    _ -> return ()
  serveEditProfile slug form enctype

serveEditProfile :: ToWidget App a
  => Slug              -- ^ Slug used to access the form
  -> a                 -- ^ Form with editable profile parameters
  -> Enctype           -- ^ Encoding type required by the form
  -> Handler Html
serveEditProfile slug form enctype = defaultLayout $ do
  setTitle "Редактирование профиля"
  $(widgetFile "edit-profile")
