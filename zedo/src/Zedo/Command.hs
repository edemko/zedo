module Zedo.Command where

import Zedo.Options
import Zedo.Find
import Zedo.Build

import System.FilePath
import System.Directory
import System.Environment hiding (setEnv)
import System.Exit
import System.Posix.Process
import Database.SQLite.Simple
import Control.Exception
import System.Process.Typed
import System.Exit


dispatch :: TopDirs -> Command -> IO ()
dispatch topDirs cmd = do
    case cmd of
        Init -> cmdInit topDirs
        Find target -> cmdFind topDirs target
        Always targets -> cmdAlways topDirs `mapM_` targets
        IfChange targets -> cmdAlways topDirs `mapM_` targets -- TODO

cmdInit :: TopDirs -> IO ()
cmdInit TopDirs{..} = do -- FIXME move this elsewhere
    -- FIXME if it already exists, prompt to re-initialize it
    createDirectoryIfMissing True workDir -- FIXME could fail
    let versionFile = workDir </> ".version"
        dbFile = workDir </> "db.sqlite3"
    withDb dbFile $ \dbFile -> do
        writeFile versionFile "zedo+sqlite v0.0.0"
        withConnection dbFile $ \db -> do
            -- TODO create the rest of the schema
            pure ()
    where
    -- TODO could I just use one transaction because the file is immediately locked?
    withDb dbFile = bracket (acquireDb dbFile) releaseDb
    acquireDb dbFile = withConnection dbFile $ \db -> do
        execute_ db "CREATE TABLE IF NOT EXISTS current_build \
                    \( pid INTEGER NOT NULL\
                    \, jobs_now INTEGER NOT NULL\
                    \, jobs_max INTEGER NOT NULL\
                    \);"
        query_ db "SELECT pid FROM current_build LIMIT 1;" >>= \case
            [Only (otherPid :: Int)] -> die $ concat ["another zedo process is already running (pid: ", show otherPid, ")"]
            [] -> pure ()
        myPid <- getProcessID
        executeNamed db "INSERT INTO current_build (pid, jobs_now, jobs_max) \
                        \VALUES (:myPid, 0, 0);" 
            [ ":myPid" := (fromIntegral myPid :: Int) ]
        pure dbFile
    releaseDb dbFile = withConnection dbFile $ \db -> do
        execute_ db "DELETE FROM current_build;"


-- FIXME should return success or failure instead of dieing
cmdFind :: TopDirs -> TargetOptions -> IO ()
cmdFind topDirs targetOpts = do
    findTargetFiles topDirs targetOpts >>= \case
        Nothing -> exitFailure
        Just TargetFiles{..} -> putStrLn targetFile


-- FIXME should return success or failure instead of dieing
cmdAlways :: TopDirs -> TargetOptions -> IO Bool
cmdAlways topDirs targetOpts = do
    targetFiles <- findTargetFiles topDirs targetOpts >>= \case
        Nothing -> exitFailure
        Just it -> pure it
    exitCode <- build topDirs targetFiles
    pure $ exitCode == ExitSuccess
