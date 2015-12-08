-- |
-- Module      :  Foundation
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- First half of boilerplate where we setup things, see "Application" for
-- the second half. We split the whole thing because of Template Haskell
-- limitations.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Foundation where

import Data.Bool (bool)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.Message
import Import.NoFoundation
import Path (fromRelDir)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod.Auth.Message (AuthMessage (InvalidLogin))
import Yesod.Core.Types (Logger)
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form.I18n.Russian (russianFormMessage)
import qualified Svod              as S
import qualified Yesod.Core.Unsafe as Unsafe

-- | The foundation datatype. This is a good place to keep settings and
-- values requiring initialization before the application starts running,
-- such as database connections. Every handler will have access to the data
-- present here.

data App = App
  { appSettings    :: AppSettings    -- ^ Application settings see "Settings"
  , appStatic      :: Static         -- ^ Settings for static file serving
  , appConnPool    :: ConnectionPool -- ^ Database connection pool
  , appHttpManager :: Manager        -- ^ HTTP manager
  , appLogger      :: Logger         -- ^ Logger settings
  }

instance HasHttpManager App where
  getHttpManager = appHttpManager

-- Define routes, see also 'mkYesodDispatch' in "Application".

mkYesodData "App" $(parseRoutesFile "config/routes")

-- | Authorization map. We keep it here for readability.

authMap
  :: Route App         -- ^ Route
  -> Bool              -- ^ Is it a write request?
  -> Handler AuthResult -- ^ Verdict
authMap LogoutR            _ = onlyUsers
authMap ChangePasswordR    _ = onlyUsers
authMap (VerifyR _)        _ = onlyUsers
authMap ProfileR           _ = onlyUsers
authMap BanUserR           _ = onlyStaff
authMap DeleteUserR        _ = onlyAdmins
authMap SubmitR            _ = onlyVerified
authMap (EditReleaseR _ _) _ = onlyVerified
authMap ApproveR           _ = onlyAdmins
authMap RejectR            _ = onlyStaff
authMap DeleteReleaseR     _ = onlyAdmins
authMap StarReleaseR       _ = onlyVerified
authMap FollowUserR        _ = onlyVerified
authMap _                  _ = return Authorized

-- | Allow access only for logged in users (possibly with unverified
-- emails).

onlyUsers :: Handler AuthResult
onlyUsers = checkWho (const $ return True) "Да ну!"

-- | Allow access only for logged in users with verified emails.

onlyVerified :: Handler AuthResult
onlyVerified = checkWho S.isVerified "Сначала нужно подтвердить адрес почты!"

-- | Allow access only for staff members (always includes admins).

onlyStaff :: Handler AuthResult
onlyStaff = checkWho S.isStaff "Только персонал Свода может это сделать."

-- | Allow access only for admins.

onlyAdmins :: Handler AuthResult
onlyAdmins = checkWho S.isAdmin "Только администратор может сделать это."

-- | Generalized check of user identity.

checkWho
  :: (UserId -> YesodDB App Bool) -- ^ Predicate
  -> Text              -- ^ Message if user doesn't satisfy the predicate
  -> Handler AuthResult -- ^ Verdict
checkWho f msg = do
  muid <- maybeAuthId
  case muid of
    Nothing -> return AuthenticationRequired
    Just u  -> runDB $ bool (Unauthorized msg) Authorized <$> f u

-- | A convenient synonym for creating forms.

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where

  -- Specify application root.

  approot = ApprootMaster (appRoot . appSettings)

  -- Store session data on the client in encrypted cookies, default session
  -- idle timeout is 120 minutes.

  makeSessionBackend _ =
    Just <$> defaultClientSessionBackend 120 "config/client_session_key.aes"

  defaultLayout widget = do
    muser  <- fmap entityVal <$> maybeAuth
    master <- getYesod
    mmsg   <- getMessage

    -- We break up the default layout into two components: default-layout is
    -- the contents of the body tag, and default-layout-wrapper is the
    -- entire page. Since the final value passed to 'hamletToRepHtml' cannot
    -- be a widget, this allows to use normal widget features in
    -- default-layout.

    pc <- widgetToPageContent $ do
      addScriptRemote "https://code.jquery.com/jquery-latest.min.js"
      $(combineStylesheets 'StaticR [css_bootstrap_min_css, css_svod_css])
      $(combineScripts     'StaticR [js_bootstrap_min_js,   js_svod_js])
      widget
    withUrlRenderer $(hamletFile "templates/default-layout.hamlet")

  -- The page to be redirected to when authentication is required.
  authRoute = const . Just $ LoginR

  isAuthorized = authMap

  -- This function creates static content files in the static folder and
  -- names them based on a hash of their content. This allows expiration
  -- dates to be set far in the future without worry of users receiving
  -- stale content.

  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      (fromRelDir staticDir)
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where genFileName lbs = "autogen-" ++ base64md5 lbs

  -- What messages should be logged. The following includes all messages
  -- when in development, and warnings and errors in production.

  shouldLog app _ level =
    appShouldLogAll (appSettings app)
      || level == LevelWarn
      || level == LevelError

  makeLogger = return . appLogger

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action (appConnPool master)

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = UserId

  loginDest         = const HomeR
  logoutDest        = const HomeR
  redirectToReferer = const True
  onLogin           = return ()
  onLogout          = return ()

  authenticate creds = runDB $ do
    user <- S.getUserBySlug . mkSlug . credsIdent $ creds
    return $ case user of
      Just (Entity uid _) -> Authenticated uid
      Nothing             -> UserError InvalidLogin

  authPlugins _   = []
  authHttpManager = error "Email doesn't need an HTTP manager"

instance YesodAuthPersist App

instance RenderMessage App FormMessage where
  renderMessage _ _ = russianFormMessage

instance RenderMessage App AuthMessage where
  renderMessage _ _ = const "Boo!"

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
