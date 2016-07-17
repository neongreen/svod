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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Users
  ( getUsersR
  , postUsersR )
where

import Handler.Register (processRegistration)
import Helper.Auth
import Helper.Json (userJson, paginatedJson)
import Import
import Svod.Search
import Widget.Pagination (lookupPagination, paginationW)
import Widget.Search (searchW, searchWidgetQueryParam)
import Widget.User (userW)
import qualified Svod as S

-- | Serve paginated list of users.

getUsersR :: Handler TypedContent
getUsersR = do
  -- Check if we've got raw search query from user.
  searchInput <- lookupGetParam searchWidgetQueryParam
  case searchInput of
    Nothing -> return ()
    Just input ->
      case parseHumanUserSearch input of
        Left _ -> return ()
        Right uqir' ->
          redirect (UsersR, renderQueryUserSearch uqir')
  -- Here we process query specified in GET params.
  pparams    <- lookupPagination
  uqir       <- lookupSearchParams
  staffHere  <- ynAuth <$> isStaff
  paginated <- runDB (S.userQuery pparams (toUserFilters staffHere uqir) [])
  selectRep $ do
    -- HTML representation
    provideRep . noHeaderLayout $ do
      setTitle "Пользователи"
      $(widgetFile "users")
    -- JSON representation
    provideRep $ do
      render <- getUrlRender
      fmap paginatedJson . forM paginated $ \user -> do
        let (Entity uid u) = user
        followers <- runDB (S.followerCount uid)
        return (userJson render followers u)

-- | Process registration and add new user.

postUsersR :: Handler TypedContent
postUsersR = processRegistration

-- | Parse search parameters from GET parameters of current request.

lookupSearchParams :: Handler Uqir
lookupSearchParams = parseQueryUserSearch . reqGetParams <$> getRequest
