-- |
-- Module      :  Handler.User
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Get information about particular user, in HTML or JSON.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.User
  ( getUserR )
where

import Data.Bool (bool)
import Helper.Access (userViaSlug)
import Import
import Widget.DngButton (BtnType (..), dngButtonW)
import Widget.FollowUser (followUserW)
import qualified Svod          as S
import qualified Text.Markdown as MD

-- | Get information about particular user in HTML or JSON.

getUserR :: Text -> Handler TypedContent
getUserR slug' =
  let slug = mkSlug slug'
  in userViaSlug slug $ \user -> do
    ownerHere  <- ynAuth <$> isSelf slug'
    adminHere  <- ynAuth <$> isAdmin
    let User {..} = entityVal user
        uid       = entityKey user
    releases    <- runDB (S.getReleasesOfUser uid)
    render      <- getUrlRender
    let userAuthor = not (null releases)
        hasStatus = or
          [ userAdmin
          , userStaff
          , userAuthor
          , not userVerified
          , userBanned && not ownerHere ]
        email = fromMaybe "-" (emailPretty userEmail)
        placeholder = StaticR $ StaticRoute ["img", "user", "placeholder.jpg"] []
    selectRep $ do
      -- HTML representation
      -- TODO Render table of releases
      provideRep . defaultLayout $ do
        setTitle (toHtml userName)
        let mdesc     = MD.markdown MD.def . fromStrict <$> userDesc
            s         = [("slug", slug')]
            verifyBtn = dngButtonW BtnSuccess "Подтвердить" s VerifyUserR
            banBtn    = dngButtonW BtnWarning
              (bool "Забанить" "Разбанить" userBanned) s BanUserR
            deleteBtn = dngButtonW BtnDanger "Удалить" s DeleteUserR
            staffBtn  = dngButtonW BtnInfo
              (bool "Нанять" "Уволить" userStaff) s MakeStaffR
            adminBtn  = dngButtonW BtnPrimary
              (bool "Возвысить" "Унизить" userAdmin) s MakeAdminR
        $(widgetFile "user")
      -- JSON representation
      -- TODO Add info about releases
      provideRep . return . object $
        maybeToList (("website" .=) <$> userWebsite)   ++
        maybeToList (("desc"    .=) <$> userDesc)      ++
        bool [] ["email" .= userEmail] userEmailPublic ++
        [ "name"     .= userName
        , "slug"     .= userSlug
        , "url"      .= render (UserR slug')
        , "joined"   .= datePretty userJoined
        , "admin"    .= userAdmin
        , "staff"    .= userStaff
        , "banned"   .= userBanned
        , "verified" .= userVerified ]
