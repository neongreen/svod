-- |
-- Module      :  Handler.Info
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Info pages (conceptual articles and the like). These articles should be
-- easily editable, so we keep them in separate repository in markdown
-- format. When we decide to deploy new versions of the articles we copy
-- them into @/static/md@ directory and then render\/serve them
-- “on-the-fly”.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Info
  ( getInfoContactR
  , getInfoTourR
  , getInfoAboutR
  , getInfoSupportSvodR
  , getInfoEulaR
  , getInfoContentR )
where

import Data.Char (isSpace)
import Import
import Path
import Text.Blaze (toMarkup)
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T
import qualified Text.Markdown     as MD

-- | Contact information.

getInfoContactR :: Handler Html
getInfoContactR = renderInfo $(mkRelFile "контактная-информация.md")

-- | Tour (short description of what you can do on the site).

getInfoTourR :: Handler Html
getInfoTourR = renderInfo $(mkRelFile "краткий-тур.md")

-- | About the Svod project.

getInfoAboutR :: Handler Html
getInfoAboutR = renderInfo $(mkRelFile "о-проекте.md")

-- | How to support the Svod project.

getInfoSupportSvodR :: Handler Html
getInfoSupportSvodR = renderInfo $(mkRelFile "поддержать-свод.md")

-- | End-user agreement.

getInfoEulaR :: Handler Html
getInfoEulaR = renderInfo $(mkRelFile "пользовательское-соглашение.md")

-- | An article about our requirements to user-submitted content.

getInfoContentR :: Handler Html
getInfoContentR = renderInfo $(mkRelFile "содержимое.md")

-- | Render info article given relative path to markdown file.

renderInfo :: Path Rel File -> Handler Html
renderInfo file = do
  articleDir <- appInfoDir . appSettings <$> getYesod
  article    <- liftIO . T.readFile . fromRelFile $ articleDir </> file
  let (title', body) = T.break (== '\n') (T.strip article) -- HACK
      title          = toMarkup (T.dropAround badChar title')
      badChar x      = x == '#' || isSpace x
  let html = MD.markdown MD.def
               { MD.msBlockFilter  = processBlocks
               , MD.msAddHeadingId = True }
               body
  defaultLayout (setTitle title >> toWidget html)

-- | Special processing for document block. This resolved internal site
-- links and adds anchors to section headers.

processBlocks = id -- TODO
