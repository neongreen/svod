-- |
-- Module      :  Foundation
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- First half of boilerplate where we setup things, see "Application" for
-- the second half. We split the whole thing because of Template Haskell
-- limitations.

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Foundation where

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

mkYesodData "App" $(parseRoutesFile "config/routes")

----------------------------------------------------------------------------
-- Tab selection

-- | Tabs shown in site menu.

data MenuTab
  = RegisterTab        -- ^ For guests: you can register here
  | LoginTab           -- ^ For guests: you can login here
  | ReleasesTab        -- ^ Search for releases
  | NotificationsTab   -- ^ See notifications
  | ProfileTab         -- ^ See or change your profile
  deriving (Show, Read, Eq, Enum, Bounded)

-- | Since our site menu has some sort of tabs, we need a way to decide
-- which of them to highlight for every route.

selectTab :: Route App -> Handler (Maybe MenuTab)
selectTab RegisterR        = return (Just RegisterTab)
selectTab LoginR           = return (Just LoginTab)
selectTab SearchReleasesR  = return (Just ReleasesTab)
selectTab NotificationsR   = return (Just NotificationsTab)
selectTab ChangePasswordR  = return (Just ProfileTab)
selectTab (UserR _)        = return (Just ProfileTab)
selectTab (UserProfileR _) = return (Just ProfileTab)
selectTab _                = return Nothing

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

  -- Allow uploads up to 500 megabytes when submitting or editing a release.

  maximumContentLength _ (Just (ReleaseR  _ _)) = Just 524288000
  maximumContentLength _ (Just (ReleasesR _  )) = Just 524288000
  maximumContentLength _ _                      = Just 2097152

  -- Store session data on the client in encrypted cookies, default session
  -- idle timeout is 120 minutes.

  makeSessionBackend _ =
    Just <$> defaultClientSessionBackend 120 "config/client_session_key.aes"

  defaultLayout = basicLayout True

  -- The page to be redirected to when authentication is required.

  authRoute = const (Just LoginR)

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

instance Semigroup (HandlerT App IO AuthResult) where
  (<>) = liftM2 ξ
    -- Combine authentication results. If one of arguments returns
    -- 'Authorized', the whole thing returns 'Authorized'. Otherwise if at
    -- least one check returns 'AuthenticationRequired', we return this
    -- given user a change to get through after login. If the both things
    -- return 'Unauthorized', we combine the messages unless they are empty.
    where ξ Authorized _             = Authorized
          ξ _ Authorized             = Authorized
          ξ AuthenticationRequired _ = AuthenticationRequired
          ξ _ AuthenticationRequired = AuthenticationRequired
          ξ (Unauthorized a) (Unauthorized b)
            | T.null a  = Unauthorized b
            | T.null b  = Unauthorized a
            | otherwise = Unauthorized (a <> " " <> b)

instance Monoid (HandlerT App IO AuthResult) where
  mempty  = return (Unauthorized T.empty)
  mappend = (<>)

-- | Almost the same as 'defaultLayout', but does not insert page header and
-- content div.

noHeaderLayout :: Widget -> Handler Html
noHeaderLayout = basicLayout False

-- | Most basic layout that allows to choose whether to generate header or
-- not.

basicLayout
  :: Bool              -- ^ Should we generate header automatically?
  -> Widget            -- ^ Widget to render
  -> Handler Html
basicLayout makeHeader widget = do
  muser     <- maybeAuth
  copyright <- appCopyright . appSettings <$> getYesod
  mroute    <- getCurrentRoute
  tab       <- maybe (return Nothing) selectTab mroute
  let registerTab = tab == Just RegisterTab
      loginTab    = tab == Just LoginTab
      releasesTab = tab == Just ReleasesTab
      notificsTab = tab == Just NotificationsTab
      profileTab  = tab == Just ProfileTab
  unseenNoti <- case entityKey <$> muser of
    Nothing -> return False
    Just uid -> runDB (S.hasUnseenNotifications uid)
  mmsg      <- getMessage
  pc        <- widgetToPageContent $ do
    cdnJQuery
    cdnBootstrap
    addStylesheet (StaticR css_svod_css)
    widget
  withUrlRenderer $(hamletFile "templates/basic-layout.hamlet")

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
