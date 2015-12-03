-- |
-- Module      :  Handler.Submit
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Submit new release for consideration.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Submit
  ( getSubmitR
  , postSubmitR )
where

import Import

-- | Serve page with “submit release” form.

getSubmitR :: Handler Html
getSubmitR = undefined

-- | Process submission.

postSubmitR :: Handler Html
postSubmitR = undefined
