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

module Handler.Users
  ( getUsersR )
where

import Import

-- | Serve paginated list of users.

getUsersR :: Handler TypedContent
getUsersR = undefined -- TODO Also in JSON
