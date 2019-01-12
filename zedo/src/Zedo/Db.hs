module Zedo.Db where

import Zedo.Find

import System.FilePath
import System.Posix.Process
import Control.Exception
import System.Exit
import Database.SQLite.Simple


newtype TargetId = TId Int
newtype Hash = Hash { unHash :: String }
    deriving (Eq, Show)
data TargetStatus
    = Unknown
    | Acquired
    | Locked
    | Ok (Maybe Hash)
    | Fail ExitCode
    deriving (Eq, Show)
statusToExitCode :: TargetStatus -> ExitCode
statusToExitCode (Ok _) = ExitSuccess
statusToExitCode (Fail ec) = ec


initDb :: Connection -> IO ()
initDb db = do
    execute_ db "CREATE TABLE target \n\
                \( id         INTEGER PRIMARY KEY\n\
                \, targetName TEXT NOT NULL UNIQUE\n\
                \, hash       TEXT\n\
                \);"
    execute_ db "CREATE TABLE targetRun \n\
                \( id         INTEGER NOT NULL UNIQUE\n\
                \, status     TEXT NOT NULL\n\
                \, exitCode   INTEGER\n\
                \, phony      BOOL NOT NULL DEFAULT 0\n\
                \, volatile   BOOL NOT NULL DEFAULT 0\n\
                \, FOREIGN KEY(id) REFERENCES target(id)\n\
                \, CONSTRAINT enum_targetRunStatus\
                \   CHECK (status IN ('lock', 'ok', 'fail'))\n\
                \, CONSTRAINT failWithExitCode\
                \   CHECK (CASE status WHEN 'fail' THEN exitCode NOT NULL ELSE exitCode IS NULL END)\n\
                \);"
                -- TODO extra

startRun :: Connection -> IO ()
startRun db = do
    execute_ db "DELETE FROM targetRun;"

startTargetRun :: Connection -> String -> IO (TargetId, TargetStatus) -- FIXME string is targetname, int is primary key
startTargetRun db targetName = do
    r <- queryNamed db "SELECT target.id, status, exitCode, hash\n\
                       \FROM target\n\
                       \    LEFT JOIN targetRun ON (target.id = targetRun.id)\n\
                       \WHERE targetName = :name;"
        [ ":name" := targetName ]
    case r of
        [(id, Just status, ec, hash)] -> pure (TId id, xformStatus status ec hash)
        [(id, Nothing, _, _)] -> do
            executeNamed db "INSERT INTO targetRun (id, status) VALUES (:id, 'lock');"
                [ ":id" := id ]
            pure (TId id, Acquired)
        [] -> do
            executeNamed db "INSERT INTO target (targetName) VALUES (:name);"
                [ ":name" := targetName ]
            [Only id] <- queryNamed db "SELECT id FROM target WHERE targetName = :name;"
                [ ":name" := targetName ]
            executeNamed db "INSERT INTO targetRun (id, status) VALUES (:id, 'lock');"
                [ ":id" := id ]
            pure (TId id, Acquired)

peekStatus :: Connection -> String -> IO TargetStatus
peekStatus db targetName = do
    r <- queryNamed db "SELECT status, exitCode, hash\n\
                       \FROM targetRun\n\
                       \    JOIN target ON (targetRun.id = target.id)\n\
                       \WHERE targetName = :targetName;"
        [ ":targetName" := targetName ]
    pure $ case r of
        [] -> Unknown
        [(status, ec, hash)] -> xformStatus status ec hash
setStatus :: Connection -> TargetId -> TargetStatus -> IO ()
setStatus db (TId id) (Ok hash) = do
    executeNamed db "UPDATE target SET hash = :hash WHERE id = :id"
        [ ":id" := id, ":hash" := unHash <$> hash ]
    executeNamed db "UPDATE targetRun SET status = 'ok' WHERE id = :id;"
        [ ":id" := id ]
setStatus db (TId id) (Fail (ExitFailure ec)) = do
    executeNamed db "UPDATE targetRun SET (status, exitCode) = ('fail', :ec) WHERE id = :id;"
        [ ":id" := id, ":ec" := ec ]

-- setHash :: Connection -> TargetId -> Maybe String -> IO ()
-- setHash db (TId id) hash = do

getPhony :: Connection -> String -> IO Bool
getPhony db targetName = do
    r <- queryNamed db "SELECT phony FROM targetRun JOIN target ON (target.id = targetRun.id) WHERE targetName = :name;"
        [ ":name" := targetName ]
    pure $ case r of
        [] -> False
        [Only b] -> b
markPhony :: Connection -> String -> IO ()
markPhony db targetName = do
    id <- queryNamed db "SELECT target.id FROM targetRun JOIN target ON (target.id = targetRun.id) WHERE targetName = :name;"
        [ ":name" := targetName ]
    case id of
        [] -> executeNamed db "INSERT INTO targetRun (targetName, phony) VALUES (:name, 1)"
                    [ ":name" := targetName ]
        [Only (id :: Int)] -> executeNamed db "UPDATE targetRun SET phony = 1 WHERE id = :id;"
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


xformStatus :: String -> Maybe Int -> Maybe String -> TargetStatus
xformStatus "lock" _ _ = Locked
xformStatus "ok" _ hash = Ok (Hash <$> hash)
xformStatus "fail" (Just ec) _ = Fail (ExitFailure ec)
xformStatus "fail" Nothing _ = error "db corruption"