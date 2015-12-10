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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}

module Foundation where

import Data.Bool (bool)
import Data.Typeable (cast)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Handler.Error (svodErrorHandler)
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
  , appPocket      :: TVar Pocket
    -- ^ HACK This is needed for decent form validation when validation of
    -- one field depends on other fields.
    --
    -- Yesod authors and maintainers think that we should do such checks
    -- after form submission and then set message in case of trouble. I
    -- think it introduces inconsistency in UI. In other words, if user
    -- enters incorrect password, it shouldn't validate and clear message
    -- /under the password/ should appear telling the user that the password
    -- is incorrect. The same with password confirmation.
  }

----------------------------------------------------------------------------
-- Pocket implementation

-- | Existentially quantified wrapper around typeable instance.

data Pocket = forall a. Typeable a => Pocket a

-- | Set pocket value.

setPocket :: Typeable a
  => a                 -- ^ Value to save
  -> HandlerT App IO ()
setPocket val = do
  ref <- getsYesod appPocket
  void . liftIO . atomically . swapTVar ref $ Pocket val

-- | Get value from the pocket.

getPocket :: Typeable a => HandlerT App IO (Maybe a)
getPocket = do
  ref <- getsYesod appPocket
  (Pocket val) <- liftIO (readTVarIO ref)
  return (cast val)

----------------------------------------------------------------------------
-- Define routes, see also 'mkYesodDispatch' in "Application".

mkYesodData "App" $(parseRoutesFile "config/routes")

-- | Authorization map. We keep it here for readability. The alternative
-- with route attributes is not type-safe and would require some additional
-- pains to get it truly right. We don't use “catch-all” pattern at the end
-- of this list to force us explicitly select authorization policy for every
-- route, most importantly we don't want to make new routes accessible by
-- default.
--
-- See also: file @config/routes@.

authm :: Route App -> Handler AuthResult

-- General stuff

authm HomeR       = return Authorized
authm FaviconR    = return Authorized
authm RobotsR     = return Authorized
authm (StaticR _) = return Authorized

-- Registration, authentication, and profile management

authm RegisterR = return Authorized
authm LoginR    = return Authorized

authm LogoutR             = isUser
authm (VerifyR _)         = isUser
authm NotificationsR      = isUser
authm (EditProfileR slug) = isAdminOrSelf slug
authm ChangePasswordR     = isUser

-- Administrative actions on users

authm VerifyUserR = isStaff
authm BanUserR    = isStaff
authm DeleteUserR = isAdmin
authm MakeStaffR  = isAdmin
authm MakeAdminR  = isAdmin

-- Public information about users (also in JSON)

authm UsersR    = return Authorized
authm (UserR _) = return Authorized

-- Actions on releases

authm SubmitReleaseR        = isVerified
authm (EditReleaseR slug _) = isAdminOrSelf slug
authm ApproveReleaseR       = isAdmin
authm RejectReleaseR        = isStaff
authm DeleteReleaseR        = isAdmin

-- Public information about releases (also in JSON)

authm ReleasesR      = return Authorized
authm (ReleaseR _ _) = return Authorized

-- Social features

authm StarReleaseR = isVerified
authm FollowUserR  = isVerified

-- Info articles

authm InfoContactR     = return Authorized
authm InfoTourR        = return Authorized
authm InfoAboutR       = return Authorized
authm InfoSupportSvodR = return Authorized
authm InfoEulaR        = return Authorized
authm InfoContentR     = return Authorized
authm InfoMarkdownR    = return Authorized

-- | Select logged-in users (possibly with unverified emails).

isUser :: Handler AuthResult
isUser = checkWho "Да ну!" (const $ return True)

-- | Select only logged-in users with verified emails.

isVerified :: Handler AuthResult
isVerified = checkWho "Сначала нужно подтвердить адрес почты!" S.isVerified

-- | Select banned users.

isBanned :: Handler AuthResult
isBanned = checkWho "Необходимо быть забанненным пользователем." S.isBanned

-- | Select staff members (always includes admins).

isStaff :: Handler AuthResult
isStaff = checkWho "Только персонал Свода может это сделать." S.isStaff

-- | Select only admins.

isAdmin :: Handler AuthResult
isAdmin = checkWho "Только администратор может сделать это." S.isAdmin

-- | Finally some pages can be accessed only by their owners or admins.

isAdminOrSelf :: Text -> Handler AuthResult
isAdminOrSelf slug =
  checkWho "Вы не имеете доступа к этой странице." $ \uid -> do
    admin <- S.isAdmin uid
    self  <- S.getUserBySlug (mkSlug slug)
    return $ admin || (entityKey <$> self) == Just uid

-- | Generalized check of user identity.

checkWho
  :: Text              -- ^ Message if user doesn't satisfy the predicate
  -> (UserId -> YesodDB App Bool) -- ^ Predicate
  -> Handler AuthResult -- ^ Verdict
checkWho msg f = do
  muid <- maybeAuthId
  case muid of
    Nothing -> return AuthenticationRequired
    Just u  -> runDB $ bool (Unauthorized msg) Authorized <$> f u

-- | Build “yes or no” variation of the functions above.

ynAuth :: AuthResult -> Bool
ynAuth Authorized = True
ynAuth _          = False

----------------------------------------------------------------------------
-- Some instances

-- | A convenient synonym for creating forms.

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance HasHttpManager App where
  getHttpManager = appHttpManager

instance Yesod App where

  -- Specify application root.

  approot = ApprootMaster (appRoot . appSettings)

  -- Customized error pages, see "Handler.Error".

  errorHandler = svodErrorHandler

  -- Store session data on the client in encrypted cookies, default session
  -- idle timeout is 120 minutes.

  makeSessionBackend _ =
    Just <$> defaultClientSessionBackend 120 "config/client_session_key.aes"

  defaultLayout widget = do
    muser  <- fmap entityVal <$> maybeAuth
    master <- getYesod
    route  <- getCurrentRoute
    let register = route == Just RegisterR
        login    = route == Just LoginR
        releases = route == Just ReleasesR
        artists  = route == Just UsersR -- FIXME
        notis    = route == Just NotificationsR
        profile  = route `elem`
          [ UserR . getSlug . userSlug <$> muser
          , EditProfileR . getSlug . userSlug <$> muser
          , Just ChangePasswordR ]
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

  isAuthorized route _ = authm route

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

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
