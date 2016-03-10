-- |
-- Module      :  Helper.Auth
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Helpers for authentication handlers. API weakness made me introduce state
-- for advanced form validation, see more about this in "Foundation".

{-# LANGUAGE NoImplicitPrelude #-}

module Helper.Auth
  ( checkUserName
  , checkUserEmail
  , checkPassStrength
  , checkPassCorrect
  , checkAuthWith )
where

import Data.Char (isLower, isUpper, isDigit)
import Import
import Yesod.Auth.Email (isValidPass)
import qualified Data.Text as T
import qualified Svod as S

-- | Check if user with given user name exists (or doesn't exist).
--
-- TODO We must also check user names so they are appropriate.

checkUserName
  :: Bool              -- ^ Desired result: should user exist?
  -> Text              -- ^ Name of the user
  -> Handler (Either Text Text)
checkUserName should name = do
  muser <- mkSlug name >>= runDB . S.getUserBySlug
  return $ case (should, muser) of
    (True,  Nothing) -> Left "Нет такого пользователя."
    (True,  Just  _) -> Right name
    (False, Nothing) -> Right name
    (False, Just  _) -> Left "Кто-то такой уже есть…"

-- | Check if given email address is already in use.

checkUserEmail :: Text -> Handler (Either Text Text)
checkUserEmail email = do
  muser <- runDB (S.getUserByEmail email)
  return $ case muser of
    Nothing -> Right email
    Just  _ -> Left "Этот адрес уже привязан к другому профилю."

-- | Too weak passwords are simply not allowed.

checkPassStrength :: Text -> Either Text Text
checkPassStrength password
  | T.length password < 10 =
    Left "Этот пароль слишком короткий, нужно минимум 10 символов."
  | isNothing (T.find isLower password) =
    Left "Нужно чтобы была хотя бы одна строчная буква."
  | isNothing (T.find isUpper password) =
    Left "Нужно чтобы была хотя бы одна заглавная буква."
  | isNothing (T.find isDigit password) =
    Left "Нужно чтобы была хотя бы одна цифра в пароле."
  | otherwise = Right password

-- | A general way to check if given password is correct.

checkPassCorrect
  :: Maybe (Entity User) -- ^ User name
  -> Text              -- ^ Password to check
  -> YesodDB App (Either Text (Entity User)) -- ^ Error message or user entity
checkPassCorrect muser given =
  let msg = Left "Этот пароль не подходит."
      ξ m f = case m of
        Nothing -> return msg
        Just a  -> f a
  in ξ muser $ \user -> do
       let uid  = entityKey user
       msalted <- S.getPassword uid
       ξ msalted $ \salted ->
         return $ if isValidPass given salted
           then Right user
           else msg

-- | Check authentication with given function and throw corresponding
-- exception in case of unauthenticated user or continue execution
-- otherwise.

checkAuthWith
  :: Handler AuthResult -- ^ Auth check to use
  -> Handler ()
checkAuthWith authCheck = do
  aresult <- authCheck
  case aresult of
    Authorized -> return ()
    AuthenticationRequired -> do
      master <- getYesod
      case authRoute master of
          Nothing -> void notAuthenticated
          Just url ->
            void . selectRep $ do
              provideRepType typeHtml $ do
                setUltDestCurrent
                void (redirect url)
              provideRepType typeJson $
                void notAuthenticated
    Unauthorized msg -> permissionDenied msg
