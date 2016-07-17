-- |
-- Module      :  Handler.Info
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Info
  ( getInfoCodecsR
  , getInfoContactR
  , getInfoLicensesR
  , getInfoAboutR
  , getInfoUserSearchR
  , getInfoReleaseSearchR
  , getInfoEulaR
  , getInfoContentR
  , getInfoMarkdownR )
where

import CMark (Node (..), NodeType (..), CMarkOption)
import Data.List (delete)
import Helper.Path (getInfoDir)
import Import hiding (delete)
import Path
import Text.Blaze.Html (preEscapedToHtml)
import Text.Read (readMaybe)
import qualified CMark        as C
import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- | Which codecs to use to prepare audio for publication.

getInfoCodecsR :: Handler Html
getInfoCodecsR = renderInfo $(mkRelFile "кодеки.md")

-- | Contact information.

getInfoContactR :: Handler Html
getInfoContactR = renderInfo $(mkRelFile "контактная-информация.md")

-- | Why licenses are important and how to choose the right one.

getInfoLicensesR :: Handler Html
getInfoLicensesR = renderInfo $(mkRelFile "лицензии.md")

-- | About the Svod project.

getInfoAboutR :: Handler Html
getInfoAboutR = renderInfo $(mkRelFile "о-проекте.md")

-- | Instructions how to search users.

getInfoUserSearchR :: Handler Html
getInfoUserSearchR = renderInfo $(mkRelFile "поиск-пользователей.md")

-- | Instructions how to search releases.

getInfoReleaseSearchR :: Handler Html
getInfoReleaseSearchR = renderInfo $(mkRelFile "поиск-публикаций.md")

-- | End-user agreement.

getInfoEulaR :: Handler Html
getInfoEulaR = renderInfo $(mkRelFile "пользовательское-соглашение.md")

-- | An article about our requirements to user-submitted content.

getInfoContentR :: Handler Html
getInfoContentR = renderInfo $(mkRelFile "контент.md")

-- | An article explaining how to use Markdown.

getInfoMarkdownR :: Handler Html
getInfoMarkdownR = renderInfo $(mkRelFile "язык-разметки.md")

-- | Render info article given relative path to markdown file.

renderInfo :: Path Rel File -> Handler Html
renderInfo file = do
  infoDir <- getInfoDir
  article <- liftIO . T.readFile . fromAbsFile $ infoDir </> file
  render  <- getUrlRender
  let node           = C.commonmarkToNode cmarkOpts article
      (title, node') = first (fromMaybe "FIXME — no title") (getTitle node)
      html  = (preEscapedToHtml . C.nodeToHtml cmarkOpts)
        (resolveInternalLinks render node')
  defaultLayout $ do
    setTitle (preEscapedToHtml title)
    cdnAnchorJs
    addScript (StaticR js_put_anchors_js)
    toWidget html

-- | Get title (technically, the first H1 header) of article. Return the
-- title 'Node' and the document with this node removed.

getTitle :: Node -> (Maybe Text, Node)
getTitle (Node pos DOCUMENT ns) =
  case find isTopHeader ns of
    Nothing -> (Nothing, Node pos DOCUMENT ns)
    Just  n ->
      let n' =
            case n of
              (Node _ (HEADING 1) [Node _ (TEXT txt) []]) -> Just txt
              _ -> Nothing
      in (n', Node pos DOCUMENT (delete n ns))
  where
    isTopHeader (Node _ (HEADING 1) _) = True
    isTopHeader _                      = False
getTitle node = (Nothing, node)

-- | Resolve internal site links. Here we try to preserve orthogonality, so
-- changes in @config/route@ don't make us change all the links in articles
-- or anywhere else. Current solution is to use names of route constructors
-- in articles and then construct routes at run-time using those names, then
-- use Yesod's native URL rendering facilities to resolve those links.

resolveInternalLinks
  :: (Route App -> Text) -- ^ The link render
  -> Node              -- ^ The input
  -> Node              -- ^ The output
resolveInternalLinks render (Node pos (LINK url title) ns) =
  let url' =
        case readMaybe (T.unpack url) :: Maybe (Route App) of
          Nothing -> url
          Just route -> render route
  in Node pos (LINK url' title) (resolveInternalLinks render <$> ns)
resolveInternalLinks render (Node pos t ns) =
  Node pos t (resolveInternalLinks render <$> ns)

-- | CMark options that are used to control markdown rendering.

cmarkOpts :: [CMarkOption]
cmarkOpts = [C.optNormalize, C.optSmart, C.optSafe]
