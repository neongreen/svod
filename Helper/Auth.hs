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
  , checkPassConfirm
  , checkPassCorrect )
where

import Data.Char (isLower, isUpper, isDigit)
import Import
import Yesod.Auth.Email (isValidPass)
import qualified Data.Text as T
import qualified Svod as S

-- | Check if user with given user name exists (or doesn't exist). This puts
-- the name into the pocket if condition is satisfied, otherwise unit @()@
-- is put into the pocket instead.
--
-- TODO We must also check user names so they are appropriate.

checkUserName
  :: Bool              -- ^ Desired result: should user exist?
  -> Text              -- ^ Name of the user
  -> Handler (Either Text Text)
checkUserName should name = do
  muser <- runDB . S.getUserBySlug . mkSlug $ name
  case (should, muser) of
    (True,  Nothing) ->
      setPocket () >> return (Left "Нет такого пользователя.")
    (True,  Just  _) ->
      setPocket name >> return (Right name)
    (False, Nothing) ->
      setPocket name >> return (Right name)
    (False, Just  _) ->
      setPocket () >> return (Left "Кто-то такой уже есть…")

-- | Check if given email address is already in use.

checkUserEmail :: Text -> Handler (Either Text Text)
checkUserEmail email = do
  muser <- runDB (S.getUserByEmail email)
  return $ case muser of
    Nothing -> Right email
    Just  _ -> Left "Этот адрес уже привязан к другому профилю."

-- | Too weak passwords are simply not allowed. This puts the given password
-- into the pocket unconditionally.

checkPassStrength :: Text -> Handler (Either Text Text)
checkPassStrength password = do
  setPocket password
  let result
        | T.length password < 10 =
          Left "Этот пароль слишком короткий, нужно минимум 10 символов."
        | isNothing (T.find isLower password) =
          Left "Нужно чтобы была хотя бы одна строчная буква."
        | isNothing (T.find isUpper password) =
          Left "Нужно чтобы была хотя бы одна заглавная буква."
        | isNothing (T.find isDigit password) =
          Left "Нужно чтобы была хотя бы одна цифра в пароле."
        | otherwise = Right password
  return result

-- | Check if given password is the same as the one in the pocket.

checkPassConfirm :: Text -> Handler (Either Text Text)
checkPassConfirm given = do
  let msg = Left "Этот пароль не совпадает с первым."
  msaved <- getPocket
  case msaved of
    Nothing -> return msg
    Just saved -> return $ if given == saved
      then Right given
      else msg

-- | A general way to check if given password is correct.

checkPassCorrect
  :: Maybe UserId      -- ^ User identifier if any
  -> Text              -- ^ Password to check
  -> Handler (Either Text Text)
checkPassCorrect muid given =
  let msg = Left "Этот пароль не подходит."
      φ m f = case m of
        Nothing -> return msg
        Just a -> f a
  in φ muid $ \uid -> do
    msalted <- runDB (S.getPassword uid)
    φ msalted $ \salted ->
      return $ if isValidPass given salted
        then Right given
        else msg
