{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad
import Data.Aeson as A
import Data.Aeson.Lens
import Data.Foldable (traverse_)
import Data.Function (on)
import qualified Data.HashMap.Lazy as HML
import Data.List (groupBy, sortOn)
import qualified Data.Ord as Ord
import qualified Data.Text as T
import Data.Time
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Forward
import GHC.Generics (Generic)
import Slick

---Config-----------------------------------------------------------------------
siteMeta :: SiteMeta
siteMeta =
  SiteMeta
    { siteAuthor = "Yair Chuchem",
      baseUrl = "https://yairchu.github.io/",
      siteTitle = "Yair's website",
      twitterHandle = Just "yairchu",
      githubUser = Just "yairchu"
    }

outputFolder :: FilePath
outputFolder = "docs/"

--Data models-------------------------------------------------------------------
withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ HML.union obj siteMetaObj
  where
    Object siteMetaObj = toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

data SiteMeta = SiteMeta
  { siteAuthor :: String,
    baseUrl :: String, -- e.g. https://example.ca
    siteTitle :: String,
    twitterHandle :: Maybe String, -- Without @
    githubUser :: Maybe String
  }
  deriving (Generic, Eq, Ord, Show, ToJSON)

-- | Data for the index page
data IndexInfo = IndexInfo
  { posts :: [Post],
    projects :: [Post],
    tagsList :: [String],
    tagName :: String
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- | Data for a blog post
data Post = Post
  { title :: String,
    author :: String,
    description :: String,
    content :: String,
    url :: String,
    date :: String,
    image :: Maybe String,
    draft :: Maybe (),
    tags :: [String]
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

data AtomData = AtomData
  { atomTitle :: String,
    domain :: String,
    atomAuthor :: String,
    atomPosts :: [Post],
    currentTime :: String,
    atomUrl :: String
  }
  deriving (Generic, ToJSON, Eq, Ord, Show)

-- | given a list of posts this will build a table of contents
buildIndex :: [Post] -> [Post] -> [String] -> Action ()
buildIndex posts' projects' allTags = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo =
        IndexInfo
          { posts = posts',
            projects = sortOn (Ord.Down . title) projects',
            tagsList = allTags,
            tagName = ""
          }
      indexHTML = T.unpack $ substitute indexT (withSiteMeta $ toJSON indexInfo)
  writeFile' (outputFolder </> "index.html") indexHTML

-- | Find and build all posts
buildPosts :: FilePath -> Action ([Post], [Post])
buildPosts folder =
  (,)
    <$> (getDirectoryFiles "." ["site/" <> folder <> "/*.md"] >>= (`forP` buildPost))
    <*> (getDirectoryFiles "." ["site/" <> folder <> "/*//*.md"] >>= (`forP` buildPost))

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath =
  cacheAction ("build" :: T.Text, srcPath) $ do
    liftIO . putStrLn $ "Rebuilding post: " <> srcPath
    postContent <- readFile' srcPath
    -- load post content and metadata as JSON blob
    let postUrl = T.pack . dropDirectory1 $ dropExtension srcPath
    postData <-
      T.pack postContent
        & markdownToHTML
        <&> _Object . at "url" ?~ String postUrl
        <&> _Object . at "lang" . filteredBy _Nothing ?~ "en"
        <&> _Object . filteredBy (at "lang") <. at "dir"
          .@~ Just . String . maybe "ltr" (const "rtl") . (^? _Just . only "he")
        <&> withSiteMeta
    -- Add additional metadata we've been able to compute
    template <- compileTemplate' "site/templates/post.html"
    let postFilename = T.unpack postUrl -<.> "html"
    writeFile' (outputFolder </> postFilename) . T.unpack $
      substitute template postData
    convert postData

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
  void $
    forP filepaths $ \filepath ->
      copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

formatDate :: String -> String
formatDate humanDate = toIsoDate parsedTime
  where
    parsedTime =
      parseTimeOrError True defaultTimeLocale "%Y.%m.%d" humanDate :: UTCTime

rfc3339 :: Maybe String
rfc3339 = Just "%H:%M:SZ"

toIsoDate :: UTCTime -> String
toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat rfc3339)

buildFeed :: [Post] -> Action ()
buildFeed ps = do
  now <- liftIO getCurrentTime
  let atomData =
        AtomData
          { atomTitle = siteTitle siteMeta,
            domain = baseUrl siteMeta,
            atomAuthor = siteAuthor siteMeta,
            atomPosts = ps <&> mkAtomPost,
            currentTime = toIsoDate now,
            atomUrl = "/atom.xml"
          }
  atomTempl <- compileTemplate' "site/templates/atom.xml"
  writeFile' (outputFolder </> "atom.xml") . T.unpack $ substitute atomTempl (toJSON atomData)
  where
    mkAtomPost :: Post -> Post
    mkAtomPost p = p {date = formatDate $ date p}

buildTag :: String -> [Post] -> Action ()
buildTag tag pages =
  do
    indexT <- compileTemplate' "site/templates/tag.html"
    let indexInfo =
          IndexInfo
            { posts = pages,
              projects = [],
              tagsList = [],
              tagName = tag
            }
        indexHTML = T.unpack $ substitute indexT (withSiteMeta $ toJSON indexInfo)
    writeFile' (outputFolder </> "tag" </> tag -<.> "html") indexHTML

makeTags :: [Post] -> [(String, [Post])]
makeTags pages =
  do
    page <- pages
    t <- tags page
    [(t, page)]
    & sortOn fst
    & groupBy ((==) `on` fst)
    <&> (,) <$> fst . head <*> sortOn (Ord.Down . date) . fmap snd

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  (allPosts, postsInner) <- buildPosts "posts" <&> _1 %~ sortOn (Ord.Down . date)
  (allProjects, projInner) <- buildPosts "projects"
  allPosts <> postsInner <> allProjects <> projInner
    & filter (has _Nothing . draft)
    & makeTags
    & traverse_ (uncurry buildTag)
  let published = filter (has _Nothing . draft) allPosts
  makeTags published
    & filter (not . null . drop 1 . snd)
    & sortOn tagOrder
    <&> fst
    & buildIndex published allProjects
  buildFeed published
  copyStaticFiles
  where
    tagOrder (t, p) = (negate (length p), length t)

main :: IO ()
main =
  shakeArgsForward shOpts buildRules
  where
    shOpts =
      shakeOptions {shakeVerbosity = Chatty, shakeLintInside = ["docs"]}
        & forwardOptions
