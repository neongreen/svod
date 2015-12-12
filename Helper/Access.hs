-- |
-- Module      :  Helper.Access
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for easy access to users and releases via slugs.

{-# LANGUAGE NoImplicitPrelude #-}

module Helper.Access
  ( userViaSlug
  , userViaSlug'
  , releaseViaSlug
  , releaseViaSlug' )
where

import Import
import qualified Svod as S

-- | Find user entity in the database by given slug. Return “404 Not Found”
-- status code (and corresponding page) on failure.

userViaSlug
  :: Slug              -- ^ User slug
  -> (Entity User -> Handler a) -- ^ How to use found data
  -> Handler a
userViaSlug slug f = do
  muser <- runDB (S.getUserBySlug slug)
  case muser of
    Nothing -> notFound
    Just user -> f user

-- | Variant of 'userViaSlug' for widgets.

userViaSlug'
  :: Slug              -- ^ User slug
  -> (Entity User -> Widget) -- ^ How to use found data
  -> Widget
userViaSlug' slug f = do
  muser <- handlerToWidget . runDB . S.getUserBySlug $ slug
  case muser of
    Nothing -> notFound
    Just user -> f user

-- | Find user and release in the database via combination of user slug and
-- release slug. If either of these is missing, return “404 Not Found”
-- status code (and corresponding page).

releaseViaSlug
  :: Slug              -- ^ Artist slug
  -> Slug              -- ^ Release slug
  -> (Entity User -> Entity Release -> Handler a) -- ^ How to use found data
  -> Handler a
releaseViaSlug uslug rslug f = userViaSlug uslug $ \user -> do
  let uid = entityKey user
  mrelease <- runDB (S.getReleaseBySlug uid rslug)
  case mrelease of
    Nothing -> notFound
    Just release -> f user release

-- | Variant of 'releaseViaSlug' for widgets.

releaseViaSlug'
  :: Slug              -- ^ Artist slug
  -> Slug              -- ^ Release slug
  -> (Entity User -> Entity Release -> Widget) -- ^ How to use found data
  -> Widget
releaseViaSlug' uslug rslug f = userViaSlug' uslug $ \user -> do
  let uid = entityKey user
  mrelease <- handlerToWidget . runDB $ S.getReleaseBySlug uid rslug
  case mrelease of
    Nothing -> notFound
    Just release -> f user release
