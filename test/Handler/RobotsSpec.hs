-- |
-- Module      :  Handler.RobotsSpec
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests for common handlers.

module Handler.RobotsSpec
  ( main
  , spec )
where

import TestImport

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $
  describe "getRobotsR" $ do
    it "gives a 200" $ do
      get RobotsR
      statusIs 200
    it "has correct User-agent" $ do
      get RobotsR
      bodyContains "User-agent: *"
