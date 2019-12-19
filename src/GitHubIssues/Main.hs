{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
module GitHubIssues.Main where

import Control.Concurrent  (threadDelay)
import Control.Exception   (IOException, catch, throwIO)
import Data.ByteString     (ByteString)
import Data.Foldable       (for_, toList, traverse_)
import Data.String         (fromString)
import Data.Text           (Text)
import Network.HTTP.Client (Manager, newManager)
import System.Directory    (createDirectoryIfMissing)
import System.Environment  (getArgs, lookupEnv)
import System.Exit         (exitFailure)
import System.FilePath     ((<.>), (</>))

import qualified Crypto.Hash.SHA256      as SHA256
import qualified Data.Binary             as Binary
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as Base16
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8   as BS8
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified GitHub                  as GH

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = GH.withOpenSSL $ lookupEnv "GITHUB_TOKEN" >>= \case
    Nothing    -> putStrLn "GITHUB_TOKEN envvar is not set" >> exitFailure
    Just _token -> getArgs >>= \case
        [owner, repo] -> doFetch (fromString owner) (fromString repo)
        _             -> putStrLn "Usage: github-issues owner repo" >> exitFailure

-------------------------------------------------------------------------------
-- Fetch
-------------------------------------------------------------------------------

doFetch :: GH.Name GH.Owner -> GH.Name GH.Repo -> IO ()
doFetch owner repo = do
    mgr <- newManager GH.tlsManagerSettings

    -- get all issues
    let issueMod :: GH.IssueRepoMod
        issueMod = GH.sortByUpdated <> GH.sortDescending <> GH.stateAll
    issues <- github mgr () $ GH.issuesForRepoR owner repo issueMod GH.FetchAll

    let dir = "issues"
    createDirectoryIfMissing True dir

    for_ issues $ \issue -> do
        let num@(GH.IssueNumber n) = GH.issueNumber issue
        let issueText :: Text
            issueText =
                "# Issue " <> tshow n <> ": " <> GH.issueTitle issue <> "\n\n"
                <> "- author: " <> showSimpleUser (GH.issueUser issue) <> "\n"
                <> "- opened: " <> tshow (GH.issueCreatedAt issue) <> "\n"
                <> "- closed: " <> maybe "" tshow (GH.issueClosedAt issue) <> "\n"
                <> "- url: " <> maybe "" GH.getUrl (GH.issueHtmlUrl issue) <> "\n"
                <> maybe "" (\b ->"\n" <> T.strip b) (GH.issueBody issue)
                <> "\n"

        comments <- github mgr () $ GH.commentsR owner repo num GH.FetchAll

        let commentText :: GH.IssueComment -> Text
            commentText c =
                "\n"
                <> "## Comment " <> tshow (GH.issueCommentId c)
                <> " by " <> showSimpleUser (GH.issueCommentUser c)
                <> " on " <> tshow (GH.issueCommentCreatedAt c)
                <> "\n\n"
                <> T.strip (GH.issueCommentBody c)
                <> "\n"

        let showN :: Int -> String
            showN m = replicate (4 - length s) '0' ++ s where s = show m
                
        BS.writeFile (dir </> showN n <.> ".txt") (TE.encodeUtf8 $ issueText <> foldMap commentText comments)

tshow :: Show a => a -> Text
tshow = T.pack . show

showSimpleUser :: GH.SimpleUser -> Text
showSimpleUser = GH.untagName . GH.simpleUserLogin

-------------------------------------------------------------------------------
-- Cached fetch
-------------------------------------------------------------------------------

class GitHubCached req res | req -> res where
    github :: GH.AuthMethod am => Manager -> am -> req -> res

instance (GH.ParseResponse mt req, Binary.Binary req, res ~ req, rw ~ 'GH.RO) => GitHubCached (GH.GenRequest mt rw req) (IO res) where
    github mgr auth req = do
        let hash :: String
            hash =
                concat [ T.unpack p ++ "-" | p <- reqPaths req ]
                ++ take 20 (BS8.unpack $ Base16.encode $ SHA256.hashlazy $ encodeReq req)
        readCache hash `catch` doesNotExist hash
      where
        readCache :: String -> IO res
        readCache hash = do
            contents <- LBS.readFile ("cache" </> hash)
            return (Binary.decode contents)

        doesNotExist :: String -> IOException -> IO res
        doesNotExist hash _ = do
            putStrLn $ "Making request for " ++ show req
            res' <- GH.executeRequestWithMgr mgr auth req
            res <- either throwIO return res'
            threadDelay 1000000 -- at most one request a second
            createDirectoryIfMissing True "cache"
            LBS.writeFile ("cache" </> hash) (Binary.encode res)
            return res

instance GitHubCached req res => GitHubCached (a -> req) (a -> res) where
    github mgr am req x = github mgr am (req x)

-------------------------------------------------------------------------------
-- Encoding
-------------------------------------------------------------------------------

reqPaths :: GH.GenRequest mt 'GH.RO req -> [Text]
reqPaths (GH.Query ps _)        = ps
reqPaths (GH.PagedQuery ps _ _) = ps

encodeReq :: GH.GenRequest mt 'GH.RO req -> LBS.ByteString
encodeReq = BSB.toLazyByteString . genReq where
    genReq (GH.Query ps qs) =
        BSB.word8 0 <> builder ps <> builder qs
    genReq (GH.PagedQuery ps qs fc) =
        BSB.word8 1 <> builder ps <> builder qs <> builder fc

class HasBuilder a where
    builder :: a -> BSB.Builder

instance HasBuilder Text where
    builder = builder . TE.encodeUtf8

instance HasBuilder ByteString where
    builder = BSB.byteString

instance (HasBuilder a, HasBuilder b) => HasBuilder (a, b) where
    builder (x, y) = builder x <> builder y

instance HasBuilder a => HasBuilder (Maybe a) where
    builder Nothing  = BSB.word8 0
    builder (Just x) = BSB.word8 1 <> builder x

instance HasBuilder a => HasBuilder [a] where
    builder xs = BSB.word8 0x12 <> foldMap builder xs

instance HasBuilder GH.FetchCount where
    builder GH.FetchAll         = BSB.word8 0
    builder (GH.FetchAtLeast n) = BSB.word8 1 <> BSB.word64BE (fromIntegral n)
