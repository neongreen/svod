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
import Handler.Error (svodErrorHandler)
import Import.NoFoundation
import Path (fromRelDir)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod.Auth.Message (AuthMessage (InvalidLogin))
import Yesod.Core.Types (Logger)
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form.I18n.Russian (russianFormMessage)
import qualified Data.Text         as T
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
authm (EditProfileR slug) = isAdmin *||* isSelf slug
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

authm SubmitReleaseR         = isVerified
authm (EditReleaseR slug _)  = isAdmin *||* isSelf slug
authm (DownloadReleaseR _ _) = isVerified

-- Administrative actions on releases

authm ApproveReleaseR = isAdmin
authm RejectReleaseR  = isStaff
authm DeleteReleaseR  = isAdmin

-- Public information about releases (also in JSON)

authm ReleasesR      = return Authorized
authm (ReleaseR _ _) = return Authorized

-- Social features

authm StarReleaseR = isVerified
authm FollowUserR  = isVerified

-- Info articles

authm InfoCodecsR      = return Authorized
authm InfoContactR     = return Authorized
authm InfoTourR        = return Authorized
authm InfoLicensesR    = return Authorized
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
isVerified = checkWho "Сначала нужно подтвердить адрес почты." S.isVerified

-- | Select banned users.

isBanned :: Handler AuthResult
isBanned = checkWho "Необходимо быть забанненным пользователем." S.isBanned

-- | Select staff members (always includes admins).

isStaff :: Handler AuthResult
isStaff = checkWho "Только персонал Свода имеет доступ." S.isStaff

-- | Select only admins.

isAdmin :: Handler AuthResult
isAdmin = checkWho "Лишь администратор имеет доступ." S.isAdmin

-- | Finally some pages may be accessed only by their owners.

isSelf :: Slug -> Handler AuthResult
isSelf slug =
  checkWho "Только владелец профиля имеет доступ." $ \uid -> do
    self <- S.getUserBySlug slug
    return $ (entityKey <$> self) == Just uid

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

-- | Combine authentication results. If one of arguments returns
-- 'Authorized', the whole thing returns 'Authorized'. Otherwise if at least
-- one check returns 'AuthenticationRequired', we return this given user a
-- change to get through after login. If the both things return
-- 'Unauthorized', we combine the messages.

infixr 2 *||*
(*||*) :: Handler AuthResult -> Handler AuthResult -> Handler AuthResult
(*||*) = liftM2 ξ
  where ξ Authorized _             = Authorized
        ξ _ Authorized             = Authorized
        ξ AuthenticationRequired _ = AuthenticationRequired
        ξ _ AuthenticationRequired = AuthenticationRequired
        ξ (Unauthorized a) (Unauthorized b) =
          Unauthorized (a <> "\n*ИЛИ*\n" <> b)

-- | Build “yes or no” variation of the functions above.

ynAuth :: AuthResult -> Bool
ynAuth Authorized = True
ynAuth _          = False

----------------------------------------------------------------------------
-- Tab selection

-- | Tabs shown in site menu.

data MenuTab
  = RegisterTab        -- ^ For guests: you can register here
  | LoginTab           -- ^ For guests: you can login here
  | ReleasesTab        -- ^ Search for releases
  | ArtistsTab         -- ^ Search for artists
  | NotificationsTab   -- ^ See notifications
  | ProfileTab         -- ^ See or change your profile
  deriving (Show, Read, Eq, Enum, Bounded)

-- | Since our site menu has some sort of tabs, we need a way to decide
-- which of them to highlight for every route.

selectTab :: Route App -> Handler (Maybe MenuTab)
selectTab RegisterR       = return (Just RegisterTab)
selectTab LoginR          = return (Just LoginTab)
selectTab ReleasesR       = return (Just ReleasesTab)
selectTab UsersR          = return (Just ArtistsTab) -- FIXME
selectTab NotificationsR  = return (Just NotificationsTab)
selectTab ChangePasswordR = return (Just ProfileTab)
selectTab (UserR slug) =
  bool Nothing (Just ProfileTab) . ynAuth <$> isSelf slug
selectTab (EditProfileR slug) =
  bool Nothing (Just ProfileTab) . ynAuth <$> isSelf slug
selectTab _ = return Nothing

----------------------------------------------------------------------------
-- Some instances

-- | A convenient synonym for creating forms.

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance HasHttpManager App where
  getHttpManager = appHttpManager

instance Yesod App where

  -- Make routes case-insensitive.

  cleanPath _ s =
    let corrected = T.toLower <$> filter (not . T.null) s
    in if corrected == s
       then Right s
       else Left corrected

  -- Specify application root.

  approot = ApprootMaster (appRoot . appSettings)

  -- Customized error pages, see "Handler.Error".

  errorHandler = svodErrorHandler

  -- Allow uploads up to 500 megabytes when submitting or editing a release.

  maximumContentLength _ (Just SubmitReleaseR)     = Just 524288000
  maximumContentLength _ (Just (EditReleaseR _ _)) = Just 524288000
  maximumContentLength _ _                         = Just 2097152

  -- Store session data on the client in encrypted cookies, default session
  -- idle timeout is 120 minutes.

  makeSessionBackend _ =
    Just <$> defaultClientSessionBackend 120 "config/client_session_key.aes"

  defaultLayout widget = do
    muser     <- fmap entityVal <$> maybeAuth
    copyright <- appCopyright . appSettings <$> getYesod
    tab       <- getCurrentRoute >>= maybe (return Nothing) selectTab
    let registerTab = tab == Just RegisterTab
        loginTab    = tab == Just LoginTab
        releasesTab = tab == Just ReleasesTab
        artistsTab  = tab == Just ArtistsTab
        notificsTab = tab == Just NotificationsTab
        profileTab  = tab == Just ProfileTab
    mmsg      <- getMessage

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

  authRoute = const (Just LoginR)

  -- See 'authm' above.

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
  onLogin           = setCsrfCookie
  onLogout          = return ()

  authenticate creds =
    case parseSlug (credsIdent creds) of
      Nothing -> return (UserError InvalidLogin)
      Just slug -> runDB $ do
        muser <- S.getUserBySlug slug
        return $ case muser of
          Just (Entity uid _) -> Authenticated uid
          Nothing -> UserError InvalidLogin

  authPlugins _   = []
  authHttpManager = error "Email doesn't need an HTTP manager"

instance YesodAuthPersist App

instance RenderMessage App FormMessage where
  renderMessage _ _ = russianFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
