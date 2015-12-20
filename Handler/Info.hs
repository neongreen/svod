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
  , getInfoLicensesR
  , getInfoAboutR
  , getInfoSupportSvodR
  , getInfoEulaR
  , getInfoContentR
  , getInfoMarkdownR )
where

import Import
import Path
import Text.Read (readMaybe)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import qualified Data.Text.Lazy.IO    as TL
import qualified Text.Markdown        as MD
import qualified Text.Markdown.Block  as MD
import qualified Text.Markdown.Inline as MD

-- | Contact information.

getInfoContactR :: Handler Html
getInfoContactR = renderInfo $(mkRelFile "контактная-информация.md")

-- | Tour (short description of what you can do on the site).

getInfoTourR :: Handler Html
getInfoTourR = renderInfo $(mkRelFile "краткий-тур.md")

-- | Why licenses are important and how to choose the right one.

getInfoLicensesR :: Handler Html
getInfoLicensesR = renderInfo $(mkRelFile "лицензии.md")

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

-- | An article explaining how to use Markdown.

getInfoMarkdownR :: Handler Html
getInfoMarkdownR = renderInfo $(mkRelFile "язык-разметки.md")

-- | Render info article given relative path to markdown file.

renderInfo :: Path Rel File -> Handler Html
renderInfo file = do
  articleDir <- appInfoDir . appSettings <$> getYesod
  article    <- liftIO . TL.readFile . fromRelFile $ articleDir </> file
  render     <- getUrlRender
  let (title', body) = TL.breakOn "\n\n" (TL.strip article)
      -- ↑ HACK we regard first paragraph as title no matter what.
      title          = MD.markdown strippingHeaders title'
      html           = MD.markdown (resolvingLinks render) body
  defaultLayout $ do
    setTitle title
    $(combineScripts 'StaticR [js_anchor_js, js_put_anchors_js])
    toWidget html

-- | Markdown rendering settings that drop headers. We use this to strip
-- possible header markdown and get title for page as plain text.

strippingHeaders :: MD.MarkdownSettings
strippingHeaders = MD.def { MD.msBlockFilter = (>>= stripHeaders) }
  where stripHeaders (MD.BlockHeading _ stuff) = [MD.BlockPlainText stuff]
        stripHeaders block = pure block

-- | Markdown rendering settings that resolve internal site links. Here we
-- try to preserve orthogonality, so changes in @config/route@ don't make us
-- change all the links in articles or anywhere else. Current solution is to
-- use names of route constructors in articles and then construct routes at
-- run-time using those names, then use Yesod's native URL rendering
-- facilities to resolve those links.

resolvingLinks
  :: (Route App -> Text) -- ^ Transform type-safe route into URL
  -> MD.MarkdownSettings -- ^ Markdown settings
resolvingLinks render = MD.def { MD.msBlockFilter = (>>= processBlock) }
  where processBlock (MD.BlockPara stuff) = [MD.BlockPara (f <$> stuff)]
        processBlock block = pure block
        f (MD.InlineLink url title content) =
          case readMaybe (T.unpack url) :: Maybe (Route App) of
            Nothing    -> MD.InlineLink url title content
            Just route -> MD.InlineLink (render route) title content
        f inline = inline
