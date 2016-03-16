-- |
-- Module      :  Handler.User.Profile
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

module Handler.User.Profile
  ( getUserProfileR
  , processUserChange )
where

import Data.Bool (bool)
import Data.Maybe (fromJust)
import Helper.Access (userViaSlug)
import Helper.Auth
import Helper.Email (startEmailVerificationCycle)
import Helper.Form
import Import
import Yesod.Form.Bootstrap3
import qualified Svod as S

-- | Editable pieces of user profile.

data UserProfileForm = UserProfileForm
  { upEmail          :: Maybe Text     -- ^ Email adderss
  , upEmailPublic    :: Maybe Bool     -- ^ Whether user's email is public
  , upTimeZoneOffset :: Maybe TimeZoneOffset -- ^ User's time zone
  , upWebsite        :: Maybe Text     -- ^ Website
  , upDesc           :: Maybe Textarea -- ^ Description (“About me”)
  }

-- | Everything user can edit in his profile is on this form. The argument
-- of the function is used to populate default values for the form.

userProfileForm :: User -> Form UserProfileForm
userProfileForm User {..} =
  renderBootstrap3 BootstrapBasicForm $ UserProfileForm
    <$> aopt emailField' (μ' "email" "Почта")
      (Just . Just . unEmail $ userEmail)
    <*> aopt checkBoxField
      (μ "email_public" $ "Публичный адрес почты (" <> email <> ")")
      (Just . Just $ userEmailPublic)
    <*> aopt (selectFieldList timeZones)
      (μ "time_zone_offset" "Часовой пояс")
      (Just . Just $ userTimeZoneOffset)
    <*> aopt urlField (μ "website" "Ваш сайт") (Just userWebsite)
    <*> aopt textareaField
      (μ "description" "Расскажите о себе")
      (Just . Textarea . unDescription <$> userDescription)
  where emailField' = check checkEmailAddress emailField
        email = emailPretty userEmail
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

getUserProfileR :: Slug -> Handler Html
getUserProfileR slug = userViaSlug slug $ \user -> do
  checkAuthWith (isAdmin <> isSelf slug)
  (form, enctype) <- generateFormPost . userProfileForm . entityVal $ user
  serveUserProfile slug form enctype

-- | Process submitted form and refresh user's profile.

processUserChange :: Slug -> Handler Html
processUserChange slug = userViaSlug slug $ \user -> do
  let u@User {..} = entityVal user
      uid         = entityKey user
  checkAuthWith (isAdmin <> isSelf slug)
  ((result, form), enctype) <- runFormPost (userProfileForm u)
  case result of
    FormSuccess UserProfileForm {..} -> do
      let newEmail = join (mkEmail <$> upEmail)
          changedEmail = isJust upEmail && Just userEmail /= newEmail
      when changedEmail $
        startEmailVerificationCycle (fromJust newEmail) userName uid
      runDB $ S.editUserProfile
        (bool Nothing newEmail changedEmail)
        upEmailPublic
        upTimeZoneOffset
        (Just upWebsite)
        (Just . mkDescription . unTextarea <$> upDesc)
        (entityKey user)
      render <- getUrlRender
      let profileUrl = render (UserR slug)
      unless changedEmail $
        setMsg MsgSuccess [shamlet|
Профиль пользователя #
<a href="#{profileUrl}">
  #{userName}
\ обновлен успешно.
|]
    _ -> return ()
  serveUserProfile slug form enctype

serveUserProfile :: ToWidget App a
  => Slug              -- ^ Slug used to access the form
  -> a                 -- ^ Form with editable profile parameters
  -> Enctype           -- ^ Encoding type required by the form
  -> Handler Html
serveUserProfile slug form enctype = defaultLayout $ do
  setTitle "Редактирование профиля"
  $(widgetFile "user-profile")
