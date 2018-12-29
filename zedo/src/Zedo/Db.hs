module Zedo.Db where

import Zedo.Find

import System.FilePath
import System.Posix.Process
import Control.Exception
import System.Exit
import Database.SQLite.Simple

initDb :: Connection -> IO ()
initDb db = do
    execute_ db "CREATE TABLE target \
                \( id         INTEGER PRIMARY KEY\n\
                \, targetName TEXT NOT NULL UNIQUE\n\
                \, exitCode   INTEGER\n\
                \, phony      BOOL NOT NULL DEFAULT 0\n\
                \, volatile   BOOL NOT NULL DEFAULT 0\n\
                \);"
                -- TODO hash, extra

startRun :: Connection -> IO ()
startRun db = do
    execute_ db "DELETE FROM target;"


getStatus :: Connection -> String -> IO (Maybe ExitCode)
getStatus db targetName = do
    r <- queryNamed db "SELECT exitCode FROM target WHERE targetName = :targetName;"
        [ ":targetName" := targetName ]
    pure $ case r of
        [] -> Nothing
        [(Only Nothing)] -> Nothing
        [(Only (Just 0))] -> Just ExitSuccess
        [(Only (Just ec))] -> Just $ ExitFailure ec
setStatus :: Connection -> String -> ExitCode -> IO ()
setStatus db targetName exit = do
    -- putStrLn $ "SET STATUS: " ++ targetName ++ " = " ++ show exit
    let ec = case exit of
            ExitSuccess -> 0
            ExitFailure ec -> ec
    id <- queryNamed db "SELECT id FROM target WHERE targetName = :name;"
        [ ":name" := targetName ]
    case id of
        [] -> executeNamed db "INSERT INTO target (targetName, exitCode) VALUES (:name, :ec)"
                    [ ":name" := targetName, ":ec" := ec ]
        [Only (id :: Int)] -> executeNamed db "UPDATE target SET exitCode = :ec WHERE id = :id;"
                    [ ":id" := id, ":ec" := ec ]

getPhony :: Connection -> String -> IO Bool
getPhony db targetName = do
    r <- queryNamed db "SELECT phony FROM target WHERE targetName = :name;"
        [ ":name" := targetName ]
    pure $ case r of
        [] -> False
        [Only b] -> b
markPhony :: Connection -> String -> IO ()
markPhony db targetName = do
    id <- queryNamed db "SELECT id FROM target WHERE targetName = :name;"
        [ ":name" := targetName ]
    case id of
        [] -> executeNamed db "INSERT INTO target (targetName, phony) VALUES (:name, 1)"
                    [ ":name" := targetName ]
        [Only (id :: Int)] -> executeNamed db "UPDATE target SET phony = 1 WHERE id = :id;"
                    [ ":id" := id ]

-- TODO could I just use one transaction because the file is immediately locked?
withNewDb :: TopDirs -> (Connection -> IO a) -> IO a
withNewDb TopDirs{..} k = do
    let dbFile = workDir </> "db.sqlite3"
    bracket (acquireDb dbFile) releaseDb $ \dbFile ->
        withConnection dbFile k
    where
    acquireDb dbFile = withConnection dbFile $ \db -> do
        execute_ db "CREATE TABLE IF NOT EXISTS current_build \
                    \( pid      INTEGER NOT NULL\
                    \, jobs_now INTEGER NOT NULL\
                    \, jobs_max INTEGER NOT NULL\
                    \, CONSTRAINT notTooManyJobs CHECK (jobs_now <= jobs_max)\
                    \);"
        query_ db "SELECT pid FROM current_build LIMIT 1;" >>= \case
            [Only (otherPid :: Int)] -> die $ concat ["another zedo process is already running (pid: ", show otherPid, ")"]
            [] -> pure ()
        myPid <- getProcessID
        executeNamed db "INSERT INTO current_build (pid, jobs_now, jobs_max) \
                        \VALUES (:myPid, 1, 1);" -- FIXME set max jobs correctly
            [ ":myPid" := (fromIntegral myPid :: Int) ]
        pure dbFile
    releaseDb dbFile = withConnection dbFile $ \db -> do
        execute_ db "DELETE FROM current_build;"

-- TODO could I just use one transaction because the file is immediately locked?
withDb :: TopDirs -> (Connection -> IO a) -> IO a
withDb TopDirs{..} k = do
    let dbFile = workDir </> "db.sqlite3"
    bracket (acquireDb dbFile) releaseDb $ \dbFile ->
        withConnection dbFile k
    where
    acquireDb dbFile = withConnection dbFile $ \db -> do
        query_ db "SELECT pid FROM current_build LIMIT 1;" >>= \case
            [Only (otherPid :: Int)] -> die $ concat ["another zedo process is already running (pid: ", show otherPid, ")"]
            [] -> pure ()
        myPid <- getProcessID
        executeNamed db "INSERT INTO current_build (pid, jobs_now, jobs_max) \
                        \VALUES (:myPid, 1, 1);" -- FIXME set max jobs correctly
            [ ":myPid" := (fromIntegral myPid :: Int) ]
        pure dbFile
    releaseDb dbFile = withConnection dbFile $ \db -> do
        execute_ db "DELETE FROM current_build;"
