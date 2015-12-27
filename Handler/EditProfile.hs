-- |
-- Module      :  Handler.EditProfile
-- Copyright   :  © 2015 Mark Karpov
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
  { epEmailPublic :: Bool
  , epWebsite     :: Maybe Text  -- ^ Website
  , epDesc        :: Maybe Textarea -- ^ Description (“About me”)
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
    <*> aopt urlField (bfs ("Ваш сайт" :: Text))
      (Just userWebsite)
    <*> aopt textareaField (bfs ("Расскажите о себе" :: Text))
      (Just . Textarea . unDescription <$> userDesc)
  where email = fromMaybe "<неверный формат>" (emailPretty userEmail)

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
