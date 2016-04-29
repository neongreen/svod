-- |
-- Module      :  Handler.FaviconSpec
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests for favicon handler.

module Handler.FaviconSpec
  ( main
  , spec )
where

import TestImport

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $
  describe "getFaviconR" $
    it "gives a 200" $ do
      get FaviconR
      statusIs 200
