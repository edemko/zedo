module Zedo.Find where

import Zedo.Options

import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import System.Environment
import Control.Exception
import System.Exit
import Control.Arrow

type Extension = String


findZedoDir :: IO (Maybe FilePath)
findZedoDir = search `catch` \(_::SomeException) -> pure Nothing
    where
    search = do
        -- WARNING: this seems to give a path without symlinks, but this is not documented
        cwd <- getCurrentDirectory
        loop cwd
    loop dir = do
        -- print dir -- FIXME use a proper logging api
        doesDirectoryExist (dir </> ".zedo") >>= \case
            True -> pure $ Just dir
            False -> if dir == "/" then pure Nothing else loop (takeDirectory dir)


data TopDirs = TopDirs
    { parent  :: Parent
    , zedoDir :: FilePath
    , srcDir  :: FilePath
    , doDir   :: FilePath
    , outDir  :: FilePath
    , distDir :: FilePath
    , workDir :: FilePath
    }
    deriving (Read, Show)

findTopDirs :: TopOptions -> IO (Maybe TopDirs)
findTopDirs TopOptions{..} = case zedoDir of
    Just zedoDir -> Just <$> findDirs zedoDir
    Nothing -> findZedoDir >>= \case
        Just zedoDir -> Just <$> findDirs zedoDir
        Nothing -> pure Nothing
    where
    findDirs zedoDir = do
        zedoDir <- makeAbsolute zedoDir
        -- FIXME these are just defaults, they should be configurable
        let srcDir  = "src"
            doDir   = "do"
            outDir  = workDir </> "build"
            distDir = "dist"
            workDir = ".zedo"
        pure TopDirs{..}


data TargetFiles = TargetFiles
    { target :: FilePath
    , targetFile :: FilePath
    , srcFile :: FilePath
    , isSource :: Bool
    , allDoFiles :: [(FilePath, Maybe Extension)]
    , doFile :: Maybe (FilePath, Maybe Extension)
    , otherDoFiles :: [(FilePath, Maybe Extension)]
    , outFile :: FilePath
    , distFile :: FilePath
    }
    deriving (Read, Show)

findTargetFiles :: TopDirs -> TargetOptions -> IO (Maybe TargetFiles)
findTargetFiles topDirs@TopDirs{..} TargetOptions{..} = maybe (pure Nothing) ioPart purePart
    where
    ioPart (target, srcFile, allDoFiles, outFile, distFile) = do
        (otherDoFiles, doFile) <- findScript allDoFiles
        let isSource = isNothing doFile
            targetFile = if isSource then srcFile else outFile
        srcOrScriptExists <- case doFile of
            Just _ -> pure True
            Nothing -> doesFileExist srcFile
        if srcOrScriptExists
            then pure $ Just TargetFiles{..}
            else die $ "no source found for: " ++ srcFile
    purePart = do
        target <- fixupDoubleDot $ case (parent, targetSpecifier) of
            (Just parent, relSpecifier)
                | "./" `isPrefixOf` relSpecifier
                || "../" `isPrefixOf` relSpecifier ->
                    let parentDir = takeDirectory parent
                    in parentDir </> relSpecifier
            (_, ('/':absSpecifier)) -> absSpecifier
            (_, absSpecifier) -> absSpecifier
        let srcFile     = srcDir       </>        target
            allDoFiles  = doDir `possibleScripts` target
            outFile     = outDir       </>        target
            distFile    = distDir      </>        target
        pure (target, srcFile, allDoFiles, outFile, distFile)


findScript :: [(FilePath, Maybe Extension)] -> IO ([(FilePath, Maybe Extension)], Maybe (FilePath, Maybe Extension))
findScript = loop []
    where
    loop acc [] = pure (reverse acc, Nothing)
    loop acc ((path, ext):paths) = doesFileExist path >>= \case
        True -> pure (reverse acc, Just (path, ext))
        False -> loop ((path, ext):acc) paths

possibleScripts :: FilePath -> FilePath -> [(FilePath, Maybe Extension)]
possibleScripts scriptRoot targetRelPath_unsafe = case fixupDoubleDot targetRelPath_unsafe of
    Nothing -> []
    Just targetRelPath -> first (normalise . (scriptRoot </>)) <$> relScripts
        where
        relScripts = (targetRelPath <.> "do", Nothing)
                    : (second Just <$> defaultScripts targetRelPath)


defaultScripts :: FilePath -> [(FilePath, Extension)]
defaultScripts targetRelPath = do
    scriptDir <- scriptDirs (takeDirectory targetRelPath)
    extention <- extensions (takeFileName targetRelPath)
    pure $ (scriptDir </> "default" <.> extention <.> "do", '.':extention)

scriptDirs :: FilePath -> [FilePath]
scriptDirs "/" = ["/"]
scriptDirs "." = ["."]
scriptDirs dirPath = dirPath : scriptDirs (takeDirectory dirPath)

extensions :: FilePath -> [String]
extensions basename = do
    let noleadingdots = dropWhile (=='.') basename
        longExt = dropWhile (/= '.') noleadingdots
    loop longExt
    where
    loop "" = []
    -- a trailing dot does _not_ mean there's a blank extension
    loop "." = []
    -- advance through /\.{2,}[^.]*/
    loop ('.':'.':rest) = loop (dropWhile (/= '.') . dropWhile (== '.') $ rest)
    -- advance through /\.[^.]+/ and take everything after the first dot
    loop ('.':rest) = rest : loop (dropWhile (/= '.') rest)

fixupDoubleDot :: FilePath -> Maybe FilePath
fixupDoubleDot filePath = loop [] (splitPath $ normalise filePath)
    where
    loop acc (up:_)
        | isUpwards up && noUpwards acc = Nothing
    loop (_:acc) (up:rest)
        | isUpwards up                  = loop acc rest
    loop acc (next:rest)                = loop (next:acc) rest
    loop acc []                         = Just . joinPath $ reverse acc
    isUpwards = (`elem` ["..", "../"])
    noUpwards = (`elem` [[], ["/"]])

makeRelativeTo = flip makeRelative
