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
  ( getInfoBusinessLogicR
  , getInfoContactR
  , getInfoTourR
  , getInfoAboutR
  , getInfoSupportSvodR
  , getInfoEulaR
  , getInfoContentR )
where

import Import
import Path
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T
import qualified Text.Markdown     as MD

-- | Business logic.

getInfoBusinessLogicR :: Handler Html
getInfoBusinessLogicR = renderInfo $(mkRelFile "бизнес-логика.md")

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

renderInfo :: Path Rel File -> Handler Html
renderInfo file = do
  articleDir <- appInfoDir . appSettings <$> getYesod
  article    <- liftIO . T.readFile . fromRelFile $ articleDir </> file
  return (MD.markdown MD.def article)
