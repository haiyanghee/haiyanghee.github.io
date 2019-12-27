{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Data.List
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Forward
import           Development.Shake.FilePath
import           GHC.Generics               (Generic)
import           Slick

import qualified Data.HashMap.Lazy as HML
import qualified Data.Text                  as T

---Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta =
    SiteMeta { siteAuthor = "Me"
             --, baseUrl = "https://example.com"
             , siteTitle = "Haiyang's blog"
             , twitterHandle = Nothing
             , githubUser = Just "haiyanghee"
             }

outputFolder :: FilePath
outputFolder = "../"

--Data models-------------------------------------------------------------------

withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ HML.union obj siteMetaObj
  where
    Object siteMetaObj = toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

data SiteMeta =
    SiteMeta { siteAuthor    :: String
             --, baseUrl       :: String -- e.g. https://example.ca
             , siteTitle     :: String
             , twitterHandle :: Maybe String -- Without @
             , githubUser    :: Maybe String
             }
    deriving (Generic, Eq, Ord, Show, ToJSON)

-- | Data for the index page
newtype IndexInfo =
  IndexInfo
    { posts :: [Post]
    } deriving (Generic, Show, FromJSON, ToJSON)

-- | Data for a blog post
data Post =
    Post { title   :: String
         , author  :: String
         , content :: String
         , description :: String
         , url     :: String
         , date    :: String
         , image   :: Maybe String
         }
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)


-- | given a list of posts this will build a table of contents
buildArchive :: [Post] -> Action ()
buildArchive posts' = do
  indexT <- compileTemplate' "site/templates/archive.html"
  let indexInfo = IndexInfo {posts = sortedPpaths posts'}
      indexHTML = T.unpack $ substitute indexT (withSiteMeta $ toJSON   indexInfo)
  writeFile' (outputFolder </> "archive.html") indexHTML


buildIndex :: [Post] -> Action ()
buildIndex posts' = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo {posts = take 5 $ sortedPpaths posts'}
      indexHTML = T.unpack $ substitute indexT (withSiteMeta $ toJSON   indexInfo)
  writeFile' (outputFolder </> "index.html") indexHTML

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
  forP pPaths buildPost

sortedPpaths :: [Post] -> [Post]
sortedPpaths = convBackToPost . sortPostTime . convPostTime . makeToPostDate

convBackToPost :: [(Post, Int)] -> [Post]
convBackToPost = fmap fst

convPostTime :: [(Post, [String])] -> [(Post, Int)]
convPostTime = fmap (\(p, e) -> (p, convertDateToDays e))

sortPostTime :: [(Post, Int)] -> [(Post, Int)]
sortPostTime = sortBy compPostTime

compPostTime :: (Post, Int) -> (Post, Int) -> Ordering
compPostTime (_, a) (_, b)  | a<b = GT
                            | a>b = LT
                            | otherwise = EQ

makeToPostDate :: [Post] -> [(Post, [String])]
makeToPostDate = fmap (\e -> (e, words $ date e))

convertDateToDays :: [String] -> Int
convertDateToDays [month,date,year]     | month == "Jan" = (read $ getRidLastElem date :: Int) +  365*(read year :: Int)
                                        | month == "Feb" = 31 + (read $ getRidLastElem date :: Int) +  365*(read year :: Int)
                                        | month == "Mar" = 31 + 28 + (read $ getRidLastElem date :: Int) +  365*(read year :: Int)
                                        | month == "Apr" = 31*2 + 28 +(read $ getRidLastElem date :: Int) +  365*(read year :: Int)
                                        | month == "May" = 31*2 + 30 + 28 + (read $ getRidLastElem date :: Int) +  365*(read year :: Int)
                                        | month == "Jun" = 31*3 + 30 + 28 + (read $ getRidLastElem date :: Int) +  365*(read year :: Int)
                                        | month == "Jul" = 31*3 + 30*2 + 28 + (read $ getRidLastElem date :: Int) +  365*(read year :: Int)
                                        | month == "Aug" = 31*4 + 30*2 + 28 + (read $ getRidLastElem date :: Int) +  365*(read year :: Int)
                                        | month == "Sep" = 31*5 + 30*2 + 28 + (read $ getRidLastElem date :: Int) +  365*(read year :: Int)
                                        | month == "Oct" = 31*5 + 30*3 + 28 + (read $ getRidLastElem date :: Int) +  365*(read year :: Int)
                                        | month == "Nov" = 31*6 + 30*3 + 28 + (read $ getRidLastElem date :: Int) +  365*(read year :: Int)
                                        | month == "Dec" = 31*6 + 30*4 + 28 + (read $ getRidLastElem date :: Int) +  365*(read year :: Int)
convertDateToDays _ = 0

getRidLastElem :: [a] -> [a]
getRidLastElem [] = []
getRidLastElem [_] = []
getRidLastElem (a:as) = a:getRidLastElem as


-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  postData <- markdownToHTML . T.pack $ postContent
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
  -- Add additional metadata we've been able to compute
  let fullPostData = withSiteMeta . withPostUrl $ postData
  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute template fullPostData
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
  allPosts <- buildPosts
  buildIndex allPosts
  buildArchive allPosts
  copyStaticFiles

main :: IO ()
main = do
  let shOpts = forwardOptions $ shakeOptions { shakeVerbosity = Chatty}
  shakeArgsForward shOpts buildRules
