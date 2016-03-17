-- |
-- Module      :  Handler.Users
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Display paginated lists of users. This supports advanced search via
-- mini-language from "Svod.Search.User".

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Users
  ( getUsersR
  , postUsersR )
where

import Handler.Register (processRegistration)
import Helper.Json (userJson, paginatedJson)
import Import
import Widget.Pagination (lookupPagination, paginationW)
import Widget.Search (searchW)
import Widget.User (userW)
import qualified Svod as S

-- | Serve paginated list of users.

getUsersR :: Handler TypedContent
getUsersR = do
  params    <- lookupPagination
  paginated <- runDB (S.userQuery params [] []) -- FIXME
  selectRep $ do
    -- HTML representation
    provideRep . noHeaderLayout $ do
      setTitle "Пользователи"
      $(widgetFile "users")
    -- JSON representation
    provideRep $ do
      render <- getUrlRender
      items  <- forM (S.paginatedItems paginated) $ \user -> do
        let (Entity uid u) = user
        followers <- runDB (S.followerCount uid)
        return (userJson render followers u)
      return (paginatedJson $ paginated { S.paginatedItems = items })

-- | Process registration and add new user.

postUsersR :: Handler TypedContent
postUsersR = processRegistration
