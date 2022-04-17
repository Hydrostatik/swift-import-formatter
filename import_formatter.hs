#!/usr/bin/env stack
-- stack runghc --resolver lts-19.4

import Control.Monad ( filterM, when, forM )
import Data.Char ( toLower )
import Data.List ( group, isPrefixOf, isSuffixOf, sortOn )
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , getCurrentDirectory
    , listDirectory
    )

ignoreDirectory :: FilePath -> Bool
ignoreDirectory x = any (`isSuffixOf` x) ["Pods", "Modules"]

ignoreFile :: FilePath -> Bool
ignoreFile _ = False

trimSuffix :: String -> String -> String
trimSuffix s w = if s `isSuffixOf` w then take (length w - length s) w else w

getAllSwiftFiles :: FilePath -> IO [FilePath]
getAllSwiftFiles filePath
  | ignoreDirectory filePath = pure []
  | otherwise = do
    contents <- listDirectory filePath
    fmap concat . forM contents $ \item -> do
      let item' = filePath <> "/" <> item
      isDir <- doesDirectoryExist item'
      if isDir
         then getAllSwiftFiles item'
         else pure [item' | ".swift" `isSuffixOf` item', not $ ignoreFile item']

fixImports :: FilePath -> IO ()
fixImports fileName = do
    contents <- lines <$> readFile fileName
    let imports = filter ("import" `isPrefixOf`) contents
    putStrLn $ "Number of imports found for" <> fileName <> ": " <> show (length imports)
    let newContents = unlines $ insertSorted imports (mconcat . removeExtraNewLines $ filter (not . isPrefixOf "import") contents)
    when (not (null newContents) && length imports > 3) $ writeFile fileName newContents
    putStrLn $ fileName <> " has been formatted!"
    where
        removeExtraNewLines lines = map (\x -> if not (null (mconcat x) && length x > 1) then x else [""]) $ group lines
        insertSorted imports contents = (\(x,y) -> x <> [""] <> sortedImports <> y) $ openImportSlot contents
            where
                openImportSlot = span ("//" `isPrefixOf`)
                sortedImports = sortOn (map toLower) imports

main :: IO ()
main = do
    parentDirectory <- trimSuffix "/script" <$> getCurrentDirectory
    fileNames <- getAllSwiftFiles parentDirectory
    mapM_ fixImports fileNames