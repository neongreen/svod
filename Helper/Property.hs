-- |
-- Module      :  Helper.Property
-- Copyright   :  © 2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Boilerplate to change properties of objects.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper.Property
  ( changeUserProperty
  , changeReleaseProperty )
where

import Helper.Access (userViaSlug, releaseViaSlug)
import Helper.Path (getFConfig)
import Import
import Svod.LTS (FConfig)
import qualified Data.Text as T

-- | Generalized action that mutates some property of users' account and
-- redirects to specified route.
--
-- PUT request should have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token in order for the operation to succeed.
--
-- The response contains JSON object specifying location (in @url@
-- attribute) where modified object (user) can be found.

changeUserProperty
  :: (UserId -> YesodDB App ()) -- ^ Database action to perform
  -> (Slug -> Route App) -- ^ Where to redirect given user's slug
  -> Slug              -- ^ 'Slug' identifying target user
  -> Handler TypedContent
changeUserProperty action route slug = do
  checkCsrfParamNamed defaultCsrfParamName
  userViaSlug slug $ \target' -> do
    let target = entityKey target'
    runDB (action target)
    render <- getUrlRender
    (return . toTypedContent . object) ["url" .= render (route slug)]

-- | Generalized version of administrative action on release. Release is
-- identified by combination of user and release slugs.
--
-- PUT request should have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token in order for the operation to succeed.
--
-- The response contains JSON object specifying location (in @url@
-- attribute) where modified object (release) can be found.

changeReleaseProperty
  :: (FConfig -> Maybe Description -> ReleaseId -> YesodDB App (Either Text a))
     -- ^ Action to perform
  -> (Slug -> Slug -> Route App) -- ^ Where to redirect given pair of slugs
  -> Slug              -- ^ Artist slug
  -> Slug              -- ^ Release slug
  -> Handler TypedContent
changeReleaseProperty action route aslug rslug = do
  checkCsrfParamNamed defaultCsrfParamName
  releaseViaSlug aslug rslug $ \_ release -> do
    let rid = entityKey release
        g x = if T.null x then Nothing else Just (mkDescription x)
    fconfig <- getFConfig
    message <- (>>= g) <$> lookupPostParam "message"
    outcome <- runDB (action fconfig message rid)
    case outcome of
      Left msg -> setMsg MsgDanger (toHtml msg)
      Right _  -> return ()
    render <- getUrlRender
    (return . toTypedContent . object) ["url" .= render (route aslug rslug)]
