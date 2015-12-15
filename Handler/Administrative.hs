-- |
-- Module      :  Handler.Administrative
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Administrative actions on users. All of these can be performed only by
-- staff and admins. All of these usually require confirmation on UI level,
-- are CSRF-protected forms that are submitted via POST requests. The
-- actions can have serious repercussions, for example deletion of user can
-- wipe his releases.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Administrative
  ( postVerifyUserR
  , postBanUserR
  , postDeleteUserR
  , postMakeStaffR
  , postMakeAdminR )
where

import Data.Bool (bool)
import Helper.Access (userViaSlug)
import Import
import qualified Svod as S

-- | Verify a user, this makes following link from email unnecessary for
-- that user.
--
-- POST request should have @"slug"@ parameter identifying user to verify
-- and 'defaultCsrfParamName' parameter containing CSRF-protection token.

postVerifyUserR :: Handler TypedContent
postVerifyUserR = postAdministrative S.setVerified UserR

-- | Toggle ban for a user. We use hellban, it's good for trolls.
--
-- POST request should have @"slug"@ parameter identifying user to ban and
-- 'defaultCsrfParamName' parameter containing CSRF-protection token.

postBanUserR :: Handler TypedContent
postBanUserR = postAdministrative toggleBanned UserR
  where toggleBanned uid = S.isBanned uid >>= S.setBanned uid . not

-- | Delete a user, use with great care.
--
-- POST request should have @"slug"@ parameter identifying user to delete
-- and 'defaultCsrfParamName' parameter containing CSRF-protection token.

postDeleteUserR :: Handler TypedContent
postDeleteUserR = postAdministrative S.deleteUser (const HomeR)

-- | Toggle user between staff member and normal user.
--
-- POST request should have @"slug"@ parameter identifying user to make
-- staff member and 'defaultCsrfParamName' parameter containing
-- CSRF-protection token.

postMakeStaffR :: Handler TypedContent
postMakeStaffR = postAdministrative toggleStaff UserR
  where toggleStaff uid = do
          staff <- S.isStaff uid
          S.setUserStatus uid (bool StaffUser NormalUser staff)

-- | Toggle user between admin and normal user, use with great care.
--
-- POST request should have @"slug"@ parameter identifying user to make
-- admin member and 'defaultCsrfParamName' parameter containing
-- CSRF-protection token.

postMakeAdminR :: Handler TypedContent
postMakeAdminR = postAdministrative toggleAdmin UserR
  where toggleAdmin uid = do
          admin <- S.isAdmin uid
          S.setUserStatus uid (bool AdminUser NormalUser admin)

-- | Generalized version of administrative action on user. User is always
-- identified by @"slug"@ parameter of POST request.

postAdministrative
  :: (UserId -> YesodDB App ()) -- ^ Action on perform on user
  -> (Text -> Route App)        -- ^ Where to redirect, given slug
  -> Handler TypedContent
postAdministrative perform route = do
  checkCsrfParamNamed defaultCsrfParamName
  slug <- runInputPost (ireq textField "slug")
  userViaSlug (mkSlug slug) $ \target' -> do
    let target = entityKey target'
    runDB (perform target)
    redirect (route slug)
