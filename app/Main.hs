{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad
import Data.Aeson as A
import Data.Aeson.Lens
import Data.List (sortOn)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Forward
import GHC.Generics (Generic)
import Slick

import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T

---Config-----------------------------------------------------------------------
siteMeta :: SiteMeta
siteMeta =
  SiteMeta
    { siteAuthor = "Yair Chuchem"
    , baseUrl = "https://yairchu.github.io/blog"
    , siteTitle = "Yair's website"
    , twitterHandle = Just "yairchu"
    , githubUser = Just "yairchu"
    }

outputFolder :: FilePath
outputFolder = "docs/"

--Data models-------------------------------------------------------------------
withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ HML.union obj siteMetaObj
  where
    Object siteMetaObj = toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

data SiteMeta =
  SiteMeta
    { siteAuthor :: String
    , baseUrl :: String -- e.g. https://example.ca
    , siteTitle :: String
    , twitterHandle :: Maybe String -- Without @
    , githubUser :: Maybe String
    }
  deriving (Generic, Eq, Ord, Show, ToJSON)

-- | Data for the index page
data IndexInfo =
  IndexInfo
    { posts :: [Post]
    , projects :: [Post]
    }
  deriving (Generic, Show, FromJSON, ToJSON)

-- | Data for a blog post
data Post =
  Post
    { title :: String
    , author :: String
    , description :: String
    , content :: String
    , url :: String
    , date :: String
    , image :: Maybe String
    , tags :: [String]
    }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

-- | given a list of posts this will build a table of contents
buildIndex :: [Post] -> [Post] -> Action ()
buildIndex posts' projects' = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo =
        IndexInfo
        { posts = reverse (sortOn date posts')
        , projects = reverse (sortOn title projects')
        }
      indexHTML = T.unpack $ substitute indexT (withSiteMeta $ toJSON indexInfo)
  writeFile' (outputFolder </> "index.html") indexHTML

-- | Find and build all posts
buildPosts :: FilePath -> Action [Post]
buildPosts folder = do
  _ <- getDirectoryFiles "." ["site/" <> folder <> "/*//*.md"] >>= (`forP` buildPost)
  getDirectoryFiles "." ["site/" <> folder <> "/*.md"] >>= (`forP` buildPost)

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath =
  cacheAction ("build" :: T.Text, srcPath) $ do
    liftIO . putStrLn $ "Rebuilding post: " <> srcPath
    postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
    postData <- markdownToHTML . T.pack $ postContent
    let postUrl = T.pack . dropDirectory1 $ dropExtension srcPath
        withPostUrl = _Object . at "url" ?~ String postUrl
  -- Add additional metadata we've been able to compute
    let fullPostData = withSiteMeta . withPostUrl $ postData
    template <- compileTemplate' "site/templates/post.html"
    let postFilename = T.unpack postUrl -<.> "html"
    writeFile' (outputFolder </> postFilename) . T.unpack $
      substitute template fullPostData
    convert fullPostData

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
  void $ forP filepaths $ \filepath ->
    copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  allPosts <- buildPosts "posts"
  allProjects <- buildPosts "projects"
  buildIndex allPosts allProjects
  copyStaticFiles

main :: IO ()
main = do
  let shOpts = forwardOptions $ shakeOptions {shakeVerbosity = Chatty}
  shakeArgsForward shOpts buildRules
