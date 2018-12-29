module Zedo.Build where

import Zedo.Find
import Zedo.Db

import Data.Maybe
import Data.List
import System.FilePath
import System.Directory
import System.IO.Temp
import System.Environment hiding (setEnv)
import System.Exit
import Control.Exception
import Control.Monad
import System.Process.Typed


build :: TopDirs -> TargetFiles -> IO ExitCode
build topDirs TargetFiles{..} = do
    status <- withDb topDirs $ \db -> getStatus db target
    case status of
        Just ec -> pure ec
        Nothing -> do
            ec <- case doFile of
                Nothing -> do
                    hasSrc <- doesFileExist srcFile
                    pure $ if hasSrc then ExitSuccess else ExitFailure 1
                Just doFile -> do
                    createDirectoryIfMissing True (takeDirectory outFile)
                    withStaging outFile $ \tmpFile -> do
                        ec <- runDoScript topDirs (target, doFile, tmpFile)
                        isPhony <- withDb topDirs $ \db -> getPhony db target
                        when (ec == ExitSuccess && not isPhony) $ renameFile tmpFile outFile
                        pure ec
            withDb topDirs $ \db -> setStatus db target ec
            pure ec


runDoScript :: TopDirs -> (FilePath, (FilePath, Maybe Extension), FilePath) -> IO ExitCode
runDoScript TopDirs{..} (target, (doFile, ext), tmpFile) = do
    let extraEnv =
            [ ("ZEDO_TARGET", target)
            , ("ZEDO__BASEDIR", zedoDir)
            ]
    targetEnv <- (extraEnv ++) <$> getEnvironment
    let args = case ext of
            Nothing -> [tmpFile]
            Just ext -> [tmpFile, fromJust $ stripSuffix ext target]
                where stripSuffix suf str = reverse <$> stripPrefix (reverse suf) (reverse str)
        build = setStdin closed
                $ setEnv targetEnv
                $ proc doFile args
    runProcess build

withStaging :: FilePath -> (FilePath -> IO a) -> IO a
withStaging outFile = bracket setup teardown
    where
    setup = do
        let (baseDir, baseFile) = splitFileName outFile
        emptyTempFile baseDir (baseFile ++ "..tmp")
    teardown tmpFile = do
        removeFile tmpFile `catch` ignoreIoErrors

ignoreIoErrors (_ :: IOError) = pure ()
