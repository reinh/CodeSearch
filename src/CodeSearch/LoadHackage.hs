{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module CodeSearch.LoadHackage where

import CodeSearch.DocumentQuery
import CodeSearch.Regex.Search
import CodeSearch.Index (DocIndex)
import CodeSearch.Document

import Data.Maybe
import Data.Text (Text)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Codec.Archive.Tar as Tar
import Data.Conduit (($=))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Conduit (simpleHttp)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Codec.Compression.GZip as GZip
import qualified CodeSearch.Index as Index
import System.FilePath.Posix (dropFileName, addExtension, joinPath, splitPath, dropTrailingPathSeparator)
import System.Posix.Files (fileExist, isDirectory, getFileStatus)
import System.Directory (getDirectoryContents, createDirectoryIfMissing)
import           Text.Regex.Base.RegexLike (MatchLength, MatchOffset)
import Control.Arrow ((&&&))

data AvailablePackages =
    AvailablePackages { runAvailPackages :: Map Text (Set Version) }
  deriving (Show)

ourPackages :: AvailablePackages
ourPackages =
  AvailablePackages $ Map.fromList
    [ ("lens", Set.fromList
                 [ -- "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1.0", "1.0.1", "1.0.2", "1.0.3", "1.1", "1.1.1", "1.2", "1.3", "1.3.1", "1.4", "1.4.1", "1.5", "1.6", "1.7", "1.7.1", "1.8", "1.9", "1.9.1", "2.0", "2.1", "2.2", "2.3", "2.4", "2.4.0.2", "2.5", "2.6", "2.6.1", "2.7", "2.7.0.1", "2.8", "2.9", "3.0", "3.0.1", "3.0.2", "3.0.3", "3.0.4", "3.0.5", "3.0.6", "3.1", "3.2", "3.3", "3.4", "3.5", "3.5.1", "3.6", "3.6.0.1", "3.6.0.2", "3.6.0.3", "3.6.0.4", "3.7", "3.7.0.1", "3.7.0.2", "3.7.1", "3.7.1.1", "3.7.1.2", "3.7.2", "3.7.3", "3.7.4", "3.7.5", "3.7.6", "3.8", "3.8.0.1", "3.8.0.2", "3.8.1", "3.8.2", "3.8.3", "3.8.4", "3.8.5", "3.8.6", "3.8.7", "3.8.7.1", "3.8.7.2", "3.8.7.3", "3.9", "3.9.0.1", "3.9.0.2", "3.9.0.3", "3.9.1", "3.9.2", "3.10", "3.10.0.1",
                            "3.10.1", "3.10.2", "4.0", "4.0.1", "4.0.2", "4.0.3", "4.0.4", "4.0.5", "4.0.6", "4.0.7", "4.1", "4.1.1", "4.1.2", "4.1.2.1"]
      )
    ]

search :: MonadIO m => DocIndex -> Text -> C.Producer m (Document, [(MatchOffset, MatchLength)], Text)
search idx rgx =
  case query idx rgx of
    Nothing -> CL.sourceList []
    Just s -> searchPackages rgx s

cabalCache :: FilePath
cabalCache = "/home/davean/.cabal/packages/hackage.haskell.org/"

ldir :: FilePath -> IO [FilePath]
ldir dr = filter (`notElem` [".", ".."]) <$> getDirectoryContents dr

downloadedPackages :: IO AvailablePackages
downloadedPackages = do
  pns <- ldir cabalCache
  ps <- forM pns $ \pn -> do
    let pdir = joinPath [cabalCache, pn]
    ds <- getFileStatus pdir
    guard (isDirectory ds)
    vs <- ldir pdir
    vl <- forM vs $ \ v -> do
                        e <- fileExist (joinPath [pdir, v, concat [pn, "-", v]] `addExtension` "tar.gz")
                        return $ if e then Just v else Nothing
    return $ Just (T.pack pn, Set.fromList . map T.pack . catMaybes $ vl)
  return . AvailablePackages . Map.fromList . filter (not . Set.null . snd) . catMaybes $ ps

enumPackages :: Monad m => AvailablePackages -> C.Producer m (Text, Version)
enumPackages packages =
  forM_ (Map.toList . runAvailPackages $ packages) $ \(pn, vs) ->
    forM_ (Set.toList vs) $ \v -> C.yield (pn, v)

buildIndex :: Monad m => C.Consumer (Document, Text) m DocIndex
buildIndex = CL.foldMap (uncurry Index.singleton)

processPackages :: MonadIO m => C.Conduit (Text, Version) m (Document, Text)
processPackages =
    CL.concatMapM gae
  where
    gae (pn, v) = do
      t <- liftIO $ getPackage pn v
      return $ explodePackage pn v t

prettyResult :: Monad m => C.Conduit (Document, [(MatchOffset, MatchLength)], Text) m Text
prettyResult =
  CL.concatMap (\(d, ms, t) ->
               let fm (s,e) = T.concat [T.pack . show $ d, " ", T.pack . show $ (s, e), "  \"... ", T.take (e+20) . T.drop (max 0 (s - 10)) $ t, " ...\""]
               in map fm ms)

searchPackages :: MonadIO m => Text
                            -> Set Document
                            -> C.Producer m (Document, [(MatchOffset, MatchLength)], Text)
searchPackages rgx ds =
  CL.sourceList (Set.toList ds) $=
    CL.map (_pName &&& _pVersion) $=
    processPackages $=
    CL.filter (\(d, _) -> Set.member d ds) $=
    CL.mapMaybe (\(d, t) ->
          case searchText rgx t of
            Left _ -> Nothing
            Right [] -> Nothing
            Right m -> Just (d, m, t))

getPackage :: Text -> Version -> IO BSL.ByteString
getPackage pn v = do
    e <- fileExist cacheFP
    if e
      then BSL.readFile cacheFP
      else do
        tb <- simpleHttp . T.unpack . T.concat $ ["http://hackage.haskell.org/package/",pn,"-",v,"/",pn,"-",v,".tar.gz"]
        createDirectoryIfMissing True (dropFileName cacheFP)
        BSL.writeFile cacheFP tb
        return tb
  where
    cacheFP = joinPath [cabalCache, T.unpack pn, T.unpack v, concat [T.unpack pn, "-", T.unpack v]] `addExtension` "tar.gz"

explodePackage :: Text -> Version -> BSL.ByteString -> [(Document, Text)]
explodePackage pn v =
    Tar.foldEntries (\e o -> he e++o) [] (const []) . Tar.read . GZip.decompress
  where
    fp2d = Document pn v
    he e = case Tar.entryContent e of
             Tar.NormalFile fc _ ->
               case TE.decodeUtf8' . BSL.toStrict $ fc of
                 Left _ -> []
                 Right tx -> [(fp2d (Tar.entryPath e), tx)]
             _ -> []

-- Adapted from cabal-db
loadAvailablePackages :: IO AvailablePackages
loadAvailablePackages =
    foldl mkMap (AvailablePackages Map.empty) . listTar . Tar.read <$> BSL.readFile "/home/davean/.cabal/packages/hackage.haskell.org/00-index.tar"
  where listTar :: Show e => Tar.Entries e -> [[FilePath]]
        listTar (Tar.Next ent nents) =
            case Tar.entryContent ent of
              Tar.NormalFile _ _ -> splitPath (Tar.entryPath ent) : listTar nents
              _                   -> listTar nents
        listTar Tar.Done             = []
        listTar (Tar.Fail err)       = error ("failed: " ++ show err)
        mkMap :: AvailablePackages -> [FilePath] -> AvailablePackages
        mkMap (AvailablePackages acc) ([packagename,packageVer,_])
            | dropTrailingPathSeparator packagename == "." = AvailablePackages acc
            | otherwise          = AvailablePackages $ tweak (T.pack . dropTrailingPathSeparator $ packagename)
                                                             (T.pack $ dropTrailingPathSeparator packageVer) acc
                        where tweak pname pver = Map.alter alterF pname
                                  where alterF Nothing  = Just (Set.singleton pver)
                                        alterF (Just z) = Just (Set.insert pver z)
        mkMap nacc _ = nacc

{-
hackageNew :: IO [(Repo, PackageName, Version)]
hackageNew = do
    fb <- simpleHttp "http://hackage.haskell.org/packages/recent.rss"
    case fmap (F.parseFeedString . T.unpack) . TE.decodeUtf8' . BSL.toStrict $ fb of
      Left _ -> fail "Text decode failed"
      Right Nothing -> fail "Hackage doesn't like us?"
      Right (Just (F.RSSFeed f)) ->
          return . map url2dp . mapMaybe F.rssItemLink . F.rssItems . F.rssChannel $ f
      _ -> fail "Wrong feed type."
  where
    url2dp :: String -> (Repo, PackageName, Version)
    url2dp p = error "No!"
-}

