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
import qualified Data.ByteString.Lazy as LBS
import qualified Crypto.Hash as Hash


build :: TopDirs -> TargetFiles -> IO ExitCode
build topDirs TargetFiles{..} = do
    (id, status) <- withDb topDirs $ \db -> startTargetRun db target
    case status of
        Ok hash -> pure ExitSuccess
        Fail ec -> pure ec
        Locked -> undefined -- TODO sleep a moment, then recurse
        Acquired -> do
            status <- case doFile of
                Nothing -> doesFileExist srcFile >>= \case
                    True -> do
                        hash <- hashFile srcFile
                        pure $ Ok (Just hash)
                    False -> pure $ Fail (ExitFailure 1)
                Just doFile -> do
                    createDirectoryIfMissing True (takeDirectory outFile)
                    withStaging outFile $ \tmpFile -> do
                        ec <- runDoScript topDirs (target, doFile, tmpFile)
                        isPhony <- withDb topDirs $ \db -> getPhony db target
                        case (ec, isPhony) of
                            (ExitSuccess, False) -> do
                                renameFile tmpFile outFile
                                hash <- hashFile outFile
                                pure $ Ok (Just hash)
                            (ExitSuccess, True) -> do
                                pure $ Ok Nothing
                            (ExitFailure ec, _) -> do
                                pure $ Fail (ExitFailure ec)
            withDb topDirs $ \db -> do
                setStatus db id status
            pure $ statusToExitCode status


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

hashFile :: FilePath -> IO Hash
hashFile filepath = do
    contents <- LBS.readFile filepath
    pure . Hash . show $ Hash.hashlazy @Hash.Tiger contents

ignoreIoErrors (_ :: IOError) = pure ()
