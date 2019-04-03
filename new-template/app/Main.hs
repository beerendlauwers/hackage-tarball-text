{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Data.Maybe (catMaybes)
import Data.List (intersperse, nub)
import Data.List.Split (splitOn)
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Control.Monad (forM, when)
import qualified Data.Map as Map
import Data.Text (Text, append, unpack, pack, replace, strip)
import Distribution.Version (Version, versionNumbers)
import Distribution.Hackage.DB (PackageData, VersionData, cabalFile, readTarball)
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.GenericPackageDescription (packageDescription)
import Distribution.Types.PackageDescription
import Distribution.Types.SourceRepo (repoLocation)
import Text.Mustache (Template, compileMustacheFile, renderMustache)
import qualified Data.Text.Lazy.IO as TIO
import Text.Show.Pretty (ppShow)

main :: IO ()
main = do
  db <- readTarball Nothing "./00-index.tar"
  t <- compileMustacheFile "./library.mustache"
--  TIO.writeFile "package.json" "[\n" -- zero out file
  objects <- forM (Map.toList db) $ \(nm, pkgData) -> do
    toMarkdown t (pack $ unPackageName nm) (Map.toList pkgData)
  metadataToJson (catMaybes objects)
--  TIO.appendFile "package.json" "\n]"

toMarkdown :: Template -> Text -> [(Version,VersionData)] -> IO (Maybe Value)
toMarkdown _ nm [] = (putStrLn $ "Package data for " ++ unpack nm ++ " was empty, skipping.") >> pure Nothing
toMarkdown template nm pkgData = do
  putStrLn $ "Trying to write package " ++ unpack nm ++ "..."
  when (elem nm []) $
    putStrLn $ show pkgData
  let latestVersion = foldl1 max $ (map fst pkgData)
  let Just latestData = packageDescription <$> (cabalFile <$> lookup latestVersion pkgData)
  let obj = object
        [ "title"   .= nm
        , "name" .= nm
        , "categories" .= (map (strip . pack) $ splitOn "," (category latestData))
        , "synopsis" .= (escape $ pack (synopsis latestData))
        , "author" .= (escape . pack $ (author latestData))
        , "maintainer" .= (escape .pack $ (maintainer latestData))
        , "hackage" .= (append "https://hackage.haskell.org/package/" nm)
        , "stackage" .= (append "https://www.stackage.org/package/" nm)
        , "source" .= (nub $ (map pack $ catMaybes (map repoLocation $ sourceRepos latestData)))
        , "versions" .= map (pack . concat . intersperse "." . map show . versionNumbers) (map fst pkgData)
        ]
  TIO.writeFile ("files/" ++ unpack nm ++ ".markdown") $ renderMustache template $ obj
  --TIO.appendFile "package.json" (encodeToLazyText obj ++ '\n')
  putStrLn $ "Wrote package " ++ unpack nm ++ "\n"
  return $ Just obj

escape :: Text -> Text
escape t =
  let escapes =
        replace "\"" "\\\"" .
        replace "\\" "\\\\"
   in escapes t

metadataToJson :: [Value] -> IO ()
metadataToJson objs = TIO.writeFile "package.json" (encodeToLazyText objs)
