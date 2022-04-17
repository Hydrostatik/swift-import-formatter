#!/usr/bin/env stack
-- stack runghc --resolver lts-19.4

import Control.Monad ( filterM, when )
import Data.Char ( toLower )
import Data.List ( group, isPrefixOf, isSuffixOf, sortOn )
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , getCurrentDirectory
    , listDirectory
    )

ignoreDirectories :: [FilePath]
ignoreDirectories = ["Pods", "Modules"]

ignoreSwiftFiles :: [FilePath]
ignoreSwiftFiles = []

trimSuffix :: String -> String -> String
trimSuffix s w = if s `isSuffixOf` w then take (length w - length s) w else w

getAllSwiftFiles :: FilePath -> IO [FilePath]
getAllSwiftFiles filePath = getAllSwiftFiles' [] (Just [filePath])

getAllSwiftFiles' :: [FilePath] -> Maybe [FilePath] -> IO [FilePath]
getAllSwiftFiles' files Nothing = return files
getAllSwiftFiles' files (Just directories) = do
    dirs <- filterM doesDirectoryExist =<< mconcat ((\x -> (fmap . fmap . fmap) (path x) listDirectory x) <$> fdirs)
    fil <- filterM doesFileExist =<< mconcat ((\x -> (fmap . fmap . fmap) (path x) listDirectory x) <$> fdirs)
    if null dirs then getAllSwiftFiles' (swiftFiles fil <> files) Nothing else getAllSwiftFiles' (swiftFiles fil <> files) (Just dirs)
    where 
        path x y = x <> "/" <> y
        fdirs = filter (\x -> not $ any (`isSuffixOf` x) ignoreDirectories) directories
        swiftFiles = filter (\x -> ".swift" `isSuffixOf` x && not (any (`isSuffixOf` x) ignoreSwiftFiles))

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