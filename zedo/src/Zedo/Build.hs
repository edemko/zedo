module Zedo.Build where

import Zedo.Find

import System.FilePath
import System.Directory
import System.Environment hiding (setEnv)
import System.Exit
import System.Process.Typed


build :: TopDirs -> TargetFiles -> IO ExitCode
build topDirs TargetFiles{..} = do
    ec <- case doFile of
        Nothing -> do
            hasSrc <- doesFileExist srcFile
            pure $ if hasSrc then ExitSuccess else ExitFailure 1
        Just doFile -> do
            let tmpFile = outFile
            createDirectoryIfMissing True (takeDirectory tmpFile)
            runDoScript topDirs (target, doFile, tmpFile)
    createDirectoryIfMissing True (takeDirectory outFile)
    pure ec


runDoScript :: TopDirs -> (FilePath, FilePath, FilePath) -> IO ExitCode
runDoScript TopDirs{..} (target, doFile, tmpFile) = do
    let extraEnv =
            [ ("ZEDO_TARGET", target)
            , ("ZEDO__BASEDIR", zedoDir)
            ]
    targetEnv <- (extraEnv ++) <$> getEnvironment
    let build = setStdin closed
                $ setEnv targetEnv
                $ proc doFile [tmpFile]
    runProcess build
