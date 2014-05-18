{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module CodeSearch.LoadHackage where

import Data.Maybe
import Data.Text (Text)
import Control.Monad
import Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Codec.Archive.Tar as Tar
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Text.Feed.Types as F
import qualified Text.Feed.Import as F
import qualified Text.RSS.Syntax as F
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Conduit (simpleHttp)
import CodeSearch.Types.Document
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set
import qualified Data.Set as Set
import Distribution.Package
import qualified Codec.Compression.GZip as GZip
import CodeSearch.Index (DocIndex)
import qualified CodeSearch.Index as Index

newtype AvailablePackages = AvailablePackages { runAvailPackages :: Map PackageName (Set Text) }

ourPackages :: AvailablePackages
ourPackages =
  AvailablePackages $ Map.fromList
    [ (PackageName "lens", Set.fromList
                 [ "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1.0", "1.0.1", "1.0.2", "1.0.3", "1.1", "1.1.1", "1.2", "1.3", "1.3.1", "1.4", "1.4.1", "1.5", "1.6", "1.7", "1.7.1", "1.8", "1.9", "1.9.1", "2.0", "2.1", "2.2", "2.3", "2.4", "2.4.0.2", "2.5", "2.6", "2.6.1", "2.7", "2.7.0.1", "2.8", "2.9", "3.0", "3.0.1", "3.0.2", "3.0.3", "3.0.4", "3.0.5", "3.0.6", "3.1", "3.2", "3.3", "3.4", "3.5", "3.5.1", "3.6", "3.6.0.1", "3.6.0.2", "3.6.0.3", "3.6.0.4", "3.7", "3.7.0.1", "3.7.0.2", "3.7.1", "3.7.1.1", "3.7.1.2", "3.7.2", "3.7.3", "3.7.4", "3.7.5", "3.7.6", "3.8", "3.8.0.1", "3.8.0.2", "3.8.1", "3.8.2", "3.8.3", "3.8.4", "3.8.5", "3.8.6", "3.8.7", "3.8.7.1", "3.8.7.2", "3.8.7.3", "3.9", "3.9.0.1", "3.9.0.2", "3.9.0.3", "3.9.1", "3.9.2", "3.10", "3.10.0.1", "3.10.1", "3.10.2", "4.0", "4.0.1", "4.0.2", "4.0.3", "4.0.4", "4.0.5", "4.0.6", "4.0.7", "4.1", "4.1.1", "4.1.2", "4.1.2.1"]
      )
    ]

allPackages :: Monad m => C.Producer m (PackageName, Version)
allPackages = 
    forM_ (Map.toList . runAvailPackages $ ourPackages) $ \(pn, vs) -> do
      forM_ (Set.toList vs) $ \v -> C.yield (pn, v)

buildIndex :: Monad m => C.Consumer (Document, Text) m DocIndex
buildIndex = CL.foldMap (uncurry Index.singleton)

processPackages :: MonadIO m => C.Conduit (PackageName, Version) m (Document, Text)
processPackages =
    CL.concatMapM gae
  where
    gae (pn, v) = do
      t <- liftIO $ getPackage pn v
      return $ explodePackage pn v t

pnm :: PackageName -> Text
pnm (PackageName n) = T.pack n

getPackage :: PackageName -> Version -> IO BSL.ByteString
getPackage pn v = do
  simpleHttp . T.unpack . T.concat $ ["http://hackage.haskell.org/package/",pnm pn,"-",v,"/",pnm pn,"-",v,".tar.gz"]

explodePackage :: PackageName -> Version -> BSL.ByteString -> [(Document, Text)]
explodePackage pn v =
    Tar.foldEntries (\e o -> he e++o) [] (const []) . Tar.read . GZip.decompress
  where
    fp2d p = Document pn v p
    he e = case Tar.entryContent e of
             Tar.NormalFile fc _ ->
               case TE.decodeUtf8' . BSL.toStrict $ fc of
                 Left _ -> []
                 Right tx -> [(fp2d (Tar.entryPath e), tx)]
             _ -> []


{-

loadAvailablePackages :: IO AvailablePackages
loadAvailablePackages = do
    tarFile <- readCabalConfig
    foldl' mkMap (AvailablePackages M.empty) . listTar . Tar.read <$> L.readFile tarFile
  where listTar :: Show e => Tar.Entries e -> [([FilePath],L.ByteString)]
        listTar (Tar.Next ent nents) =
            case Tar.entryContent ent of
              Tar.NormalFile bs _ -> (splitPath $ Tar.entryPath ent, bs) : listTar nents
              _                   -> listTar nents
        listTar Tar.Done             = []
        listTar (Tar.Fail err)       = error ("failed: " ++ show err)
        mkMap :: AvailablePackages -> ([FilePath], L.ByteString) -> AvailablePackages
        mkMap (AvailablePackages acc) ([(dropTrailingPathSeparator -> packagename),packageVer,_],entBS)
            | packagename == "." = AvailablePackages acc
            | otherwise          = AvailablePackages $ tweak (PackageName packagename)
                                                             (fromString $ dropTrailingPathSeparator packageVer)
                                                             entBS acc
                        where tweak !pname !pver !cfile !m = M.alter alterF pname m
                                  where alterF Nothing  = Just [(pver,cfile)]
                                        alterF (Just z) = Just ((pver,cfile) : z)
        mkMap nacc _ = nacc

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
