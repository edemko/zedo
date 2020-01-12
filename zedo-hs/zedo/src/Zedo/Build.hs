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


changed :: TopDirs -> FilePath -> IO Bool
changed topDirs targetName = withDb topDirs (\db -> targetInfo db targetName) >>= \case
    Nothing -> do
        -- putErrLn $ show targetName ++ " changed: unknown target"
        pure True
    Just (_, _, Right status) -> case status of
        Locked -> do
            -- putErrLn $ show targetName ++ " changed: Locked status"
            pure True
        Fail _ -> do
            -- putErrLn $ show targetName ++ " changed: Fail status"
            pure True
        Ok _ -> do
            -- putErrLn $ show targetName ++ " not changed: Ok status"
            pure False
        -- Acquired -- should never happen
        -- Unknown -- should never happen
    Just (id, _, Left Nothing) -> do
        -- putErrLn $ show targetName ++ " changed: no previous hash"
        pure True
    Just (id, location, Left (Just lastHash)) ->
        let file = case location of
                    Source -> srcDir topDirs </> targetName
                    Output -> outDir topDirs </> targetName
                    Script -> doDir topDirs </> targetName
        in doesFileExist file >>= \case
            False -> do
                -- putErrLn $ show targetName ++ " changed: output file " ++ show file ++ " does not exist"
                pure True
            True -> (lastHash /=) <$> hashFile file >>= \case
                True -> do
                    -- putErrLn $ show targetName ++ " changed: hash differs"
                    pure True
                False -> do
                    deps <- withDb topDirs $ \db -> peekDependencies db targetName
                    childrenChanged <- forM deps $ \(depType, childName) -> case depType of
                        Change -> changed topDirs childName
                        Create -> created topDirs childName
                    let anyChildChange = or childrenChanged
                    -- if anyChildChange
                    --     then putErrLn $ show targetName ++ " changed: children changed"
                    --     else putErrLn $ show targetName ++ " not changed: hash matches and children unchanged"
                    unless anyChildChange $ do
                        -- putErrLn $ "saving Ok state for " ++ show targetName
                        withDb topDirs $ \db ->
                            setStatus db id (Ok (Just lastHash))
                    pure $ anyChildChange

created :: TopDirs -> FilePath -> IO Bool
created topDirs targetName = do
    r <- withDb topDirs (\db -> targetInfo db targetName) >>= \case
        Nothing -> pure True
        Just (_, _, Right status) -> pure True
        Just (_, Source, Left _) -> doesFileExist $ srcDir topDirs </> targetName
        Just (_, Output, Left _) -> doesFileExist $ outDir topDirs </> targetName
        Just (_, Script, Left _) -> doesFileExist $ doDir topDirs </> targetName
    -- if r
    -- then putErrLn $ show targetName ++ " created"
    -- else putErrLn $ show targetName ++ " not created"
    pure r


build :: TopDirs -> TargetFiles -> IO ExitCode
build topDirs TargetFiles{..} = do
    let location = maybe Source (const Output) doFile
    (id, status) <- withDb topDirs $ \db -> startTargetRun db (location, target)
    status <- case status of
        status@(Ok hash) -> pure status
        status@(Fail ec) -> pure status
        Locked -> undefined -- TODO sleep a moment, then recurse
        Acquired -> case doFile of
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
        case parent topDirs of
            Nothing -> pure ()
            Just parent -> saveDep db Change parent target
        case doFile of
            Nothing -> pure ()
            Just (doFile, _, _) -> do
                scriptHash <- hashFile $ doDir topDirs </> ((\(ZedoPath x) -> x) doFile)
                saveScriptDep db Change (Just scriptHash) target doFile
        forM_ otherDoFiles $ \(doFile, _, _) -> saveScriptDep db Create Nothing target doFile
        setStatus db id status
    pure $ statusToExitCode status


runDoScript :: TopDirs -> (FilePath, (ZedoPath, FilePath, Maybe Extension), FilePath) -> IO ExitCode
runDoScript TopDirs{..} (target, (_, doFile, ext), tmpFile) = do
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
