-- |
-- Module      :  Handler.User
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GNU GPL version 3
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
import Helper.Rendering (renderDescription, toInt)
import Import
import Widget.DngButton (BtnType (..), dngButtonW)
import Widget.FollowUser (followUserW)
import Widget.Release (releaseW)
import qualified Svod as S

-- | Get information about particular user in HTML or JSON.

getUserR :: Slug -> Handler TypedContent
getUserR slug = userViaSlug slug $ \user -> do
  ownerHere <- ynAuth <$> isSelf slug
  staffHere <- ynAuth <$> isStaff
  adminHere <- ynAuth <$> isAdmin
  let User {..} = entityVal user
      userAdmin = userStatus == AdminUser
      userStaff = userStatus `elem` [StaffUser, AdminUser]
      uid       = entityKey user
  releases  <- runDB (S.getReleasesOfUser uid)
  render    <- getUrlRender
  let userAuthor = not (null releases)
      hasStatus = or
        [ userAdmin
        , userStaff
        , userAuthor
        , not userVerified
        , userBanned && staffHere ]
      email = fromMaybe "-" (emailPretty userEmail)
      placeholder = StaticR $ StaticRoute ["img", "user", "placeholder.jpg"] []
  selectRep $ do
    -- HTML representation
    provideRep . defaultLayout $ do
      setTitle (toHtml userName)
      $(widgetFile "user")
    -- JSON representation
    provideRep $ do
      stars <- mapM (runDB . S.starCount . entityKey) releases
      return . object $
        maybeToList (("website" .=) <$> userWebsite)   ++
        maybeToList (("desc"    .=) <$> userDesc)      ++
        bool [] ["email" .= userEmail] userEmailPublic ++
        [ "name"     .= userName
        , "slug"     .= userSlug
        , "url"      .= render (UserR slug)
        , "joined"   .= datePretty userJoined
        , "admin"    .= userAdmin
        , "staff"    .= userStaff
        , "banned"   .= userBanned
        , "verified" .= userVerified
        , "releases" .= (f <$> zip stars releases) ]
        where f (s, e) =
                let x = entityVal e
                in object
                  [ "title" .= releaseTitle x
                  , "year"  .= toInt (releaseYear x)
                  , "stars" .= toInt s ]
