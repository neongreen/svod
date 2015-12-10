-- |
-- Module      :  Handler.SubmitRelease
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Submit new release for consideration.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.SubmitRelease
  ( getSubmitReleaseR
  , postSubmitReleaseR )
where

import Import

-- | Serve page with “submit release” form.

getSubmitReleaseR :: Handler Html
getSubmitReleaseR = undefined

-- | Process submission.

postSubmitReleaseR :: Handler Html
postSubmitReleaseR = undefined
