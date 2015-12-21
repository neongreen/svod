-- |
-- Module      :  Helper.Auth
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
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
  , checkPassCorrect )
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
  muser <- runDB . S.getUserBySlug . mkSlug $ name
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
      φ m f = case m of
        Nothing -> return msg
        Just a  -> f a
  in φ muser $ \user -> do
       let uid  = entityKey user
       msalted <- S.getPassword uid
       φ msalted $ \salted ->
         return $ if isValidPass given salted
           then Right user
           else msg
