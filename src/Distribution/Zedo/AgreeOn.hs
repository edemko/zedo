module Distribution.Zedo.AgreeOn where

import Data.Maybe

import Data.Functor
import Control.Monad.Reader
import Control.Monad.IO.Class

import System.FilePath
import System.Directory

import Distribution.Zedo.Data


invokerVar = "ZEDO__INVOKER"

baseDirVar = "ZEDO__baseDir"

verbosityVar = "ZEDO__verbosity"


findWorkDir :: FilePath -> FilePath
findWorkDir = (</> ".zedo")

findDbFile :: FilePath -> FilePath
findDbFile = findWorkDir .> (</> "db.sqlite")


findBaseDir :: MonadIO m => m (Maybe FilePath)
findBaseDir = do
    pwd <- liftIO getCurrentDirectory
    let candidates = takeUntilFixedBy equalFilePath $ iterate takeDirectory pwd
    loop candidates
    where
    loop :: MonadIO m => [FilePath] -> m (Maybe FilePath)
    loop [] = pure Nothing
    loop (candidate:rest) = checkBaseDir candidate >>= \case
        Nothing -> loop rest
        result -> pure result

checkBaseDir :: MonadIO m => FilePath -> m (Maybe FilePath)
checkBaseDir candidate = liftIO $ do
    result <- normalise <$> makeAbsolute candidate
    -- FIXME normalise doesn't remove trailing slash
    doesDirectoryExist (result </> ".zedo") <&> \case
        True -> Just result
        False -> Nothing

findTarget :: (MonadReader InvocationInvariants m, MonadIO m)
                => FilePath -> m AtomInvariants
findTarget = \case
    '/':abspath -> pure $ build abspath
    relpath -> asks invoker <&> \case
        Just (TargetPath parent) -> build $ parent </> relpath
        Nothing -> build relpath
    where
    build = AI . TargetPath . normalise

candidateScripts :: TargetPath -> [ScriptSpec]
candidateScripts (TargetPath path) =
    let specificScript = (path <.> "do", Nothing)
        defaultScripts = [ (dir </> "default" <.> ext <.> "do", Just ext)
                         | dir <- genDirs $ takeDirectory path
                         , ext <- genExts $ takeFileName path
                         ]
    in genSpecs [] $ specificScript : defaultScripts
    where
    genExts "" = []
    genExts filename =
        let preExts = dropWhile (== '.') filename
            (_, exts) = break (== '.') preExts
        in case exts of
            '.':'.':exts -> genExts exts
            '.':exts -> exts : genExts exts
            _ -> []
    genDirs dir0 = takeUntilFixedBy equalFilePath $ iterate takeDirectory dir0
    genSpecs :: [ScriptPath] -> [(FilePath, Maybe String)] -> [ScriptSpec]
    genSpecs seen [] = ScriptSpec
        { scriptPath = Nothing
        , scriptExtension = Nothing
        , notScriptPaths = seen
        } : []
    genSpecs seen ((prePath, ext) : rest) =
        let path = ScriptPath $ normalise prePath
        in ScriptSpec
            { scriptPath = Just path
            , scriptExtension = ext
            , notScriptPaths = seen
            } : genSpecs (seen ++ [path]) rest





takeUntilFixed :: Eq a => [a] -> [a]
takeUntilFixed = takeUntilFixedBy (==)
takeUntilFixedBy :: (a -> a -> Bool) -> [a] -> [a]
takeUntilFixedBy eq [] = []
takeUntilFixedBy eq [a] = [a]
takeUntilFixedBy eq (a:b:rest)
    | a `eq` b = [a]
    | otherwise = a : takeUntilFixedBy eq (b:rest)

(.>) = flip (.)