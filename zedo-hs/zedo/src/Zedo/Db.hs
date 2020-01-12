module Zedo.Db where

import Zedo.Find

import System.FilePath
import System.Posix.Process
import Control.Exception
import System.Exit
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import qualified Data.Text as T


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

data DepType = Change | Create
    deriving(Read, Show, Eq)
data TargetLoc = Source | Output | Script
    deriving(Read, Show, Eq)


initDb :: Connection -> IO ()
initDb db = do
    execute_ db "CREATE TABLE target\n\
                \( id         INTEGER PRIMARY KEY\n\
                \, location   TEXT NOT NULL\n\
                \, targetName TEXT NOT NULL UNIQUE\n\
                \, hash       TEXT\n\
                \, CONSTRAINT enum_location CHECK (location IN ('Source', 'Output', 'Script'))\n\
                \);"
    execute_ db "CREATE TABLE targetRun\n\
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
    execute_ db "CREATE TABLE dependency\n\
                \( deptype   TEXT NOT NULL\n\
                \, parent_id INTEGER NOT NULL REFERENCES target(id)\n\
                \, child_id  INTEGER NOT NULL REFERENCES target(id)\n\
                \, UNIQUE (deptype, parent_id, child_id)\n\
                \, CONSTRAINT enum_deptype CHECK (deptype IN ('Change', 'Create'))\n\
                \);"
                -- TODO extra

startRun :: Connection -> IO ()
startRun db = do
    execute_ db "DELETE FROM targetRun;"

startTargetRun :: Connection -> (TargetLoc, String) -> IO (TargetId, TargetStatus) -- FIXME string is targetname, int is primary key
startTargetRun db (location, targetName) = do
    r <- queryNamed db "SELECT target.id, status, exitCode, hash\n\
                       \FROM target\n\
                       \    LEFT JOIN targetRun ON (target.id = targetRun.id)\n\
                       \WHERE targetName = :name AND location = :location;"
        [ ":name" := targetName, ":location" := show location ]
    case r of
        [(id, Just status, ec, hash)] -> pure (TId id, xformStatus status ec hash)
        [(id, Nothing, _, _)] -> do
            executeNamed db "INSERT INTO targetRun (id, status) VALUES (:id, 'lock');"
                [ ":id" := id ]
            executeNamed db "DELETE FROM dependency WHERE parent_id = :id;"
                [ ":id" := id ]
            pure (TId id, Acquired)
        [] -> do
            executeNamed db "INSERT INTO target (targetName, location) VALUES (:name, :location);"
                [ ":name" := targetName, ":location" := show location ]
            [Only id] <- queryNamed db "SELECT id FROM target WHERE targetName = :name;"
                [ ":name" := targetName ]
            executeNamed db "INSERT INTO targetRun (id, status) VALUES (:id, 'lock');"
                [ ":id" := id ]
            pure (TId id, Acquired)

targetInfo :: Connection -> String -> IO (Maybe (TargetId, TargetLoc, Either (Maybe Hash) TargetStatus))
targetInfo db targetName = do
    r <- queryNamed db "SELECT target.id, location, hash, status, exitCode\n\
                       \FROM target\n\
                       \    LEFT JOIN targetRun ON (targetRun.id = target.id)\n\
                       \WHERE targetName = :targetName;"
        [ ":targetName" := targetName ]
    pure $ case r of
        [] -> Nothing
        [(id, loc, hash, Nothing, _)] ->
            Just (TId id, read loc, Left $ Hash <$> hash)
        [(id, loc, hash, Just status, ec)] ->
            Just (TId id, read loc, Right $ xformStatus status ec hash)

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
    executeNamed db "INSERT OR IGNORE INTO targetRun (id, status) VALUES (:id, 'ok');"
        [ ":id" := id ]
    executeNamed db "UPDATE targetRun SET status = 'ok' WHERE id = :id;"
        [ ":id" := id ]
setStatus db (TId id) (Fail (ExitFailure ec)) = do
    executeNamed db "UPDATE targetRun SET (status, exitCode) = ('fail', :ec) WHERE id = :id;"
        [ ":id" := id, ":ec" := ec ]

saveDep :: Connection -> DepType -> String -> String -> IO ()
saveDep db deptype parent child = do
    [(Only (pid :: Int))] <- queryNamed db "SELECT id FROM target WHERE targetName = :name;"
        [ ":name" := parent ]
    [(Only (cid :: Int))] <- queryNamed db "SELECT id FROM target WHERE targetName = :name;"
        [ ":name" := child ]
    executeNamed db "INSERT OR IGNORE INTO dependency (deptype, parent_id, child_id)\n\
                    \VALUES (:deptype, :pid, :cid);"
        [ ":deptype" := show deptype, ":pid" := pid, ":cid" := cid ]

saveScriptDep :: Connection -> DepType -> Maybe Hash -> String -> ZedoPath -> IO ()
saveScriptDep db deptype hash parent (ZedoPath child) = do
    [(Only (pid :: Int))] <- queryNamed db "SELECT id FROM target WHERE targetName = :name AND location != 'Script';"
        [ ":name" := parent ]
    r <- queryNamed db "SELECT id FROM target WHERE targetName = :name AND location = 'Script';"
        [ ":name" := child ]
    (TId cid) <- case r of
        [(Only id)] -> pure $ TId id
        [] -> do
            executeNamed db "INSERT INTO target (targetName, location) VALUES (:name, 'Script');"
                [ ":name" := child ]
            [Only id] <- queryNamed db "SELECT id FROM target WHERE targetName = :name;"
                [ ":name" := child ]
            pure $ TId id
    executeNamed db "UPDATE target SET hash = :hash WHERE id = :cid;"
        [ ":cid" := cid, ":hash" := unHash <$> hash ]
    executeNamed db "INSERT OR IGNORE INTO dependency (deptype, parent_id, child_id)\n\
                    \VALUES (:deptype, :pid, :cid);"
        [ ":deptype" := show deptype, ":pid" := pid, ":cid" := cid ]

peekDependencies :: Connection -> String -> IO [(DepType, String)]
peekDependencies db targetName = do
    queryNamed db "SELECT depType, child.targetName\n\
                  \FROM dependency\n\
                  \    JOIN target AS parent ON (parent_id = parent.id)\n\
                  \    JOIN target AS child ON (child_id = child.id)\n\
                  \WHERE parent.targetName = :name;"
        [ ":name" := targetName ]

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
        [] -> executeNamed db "INSERT INTO targetRun (targetName, phony) VALUES (:name, 1);"
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

instance FromField DepType where
    fromField field = case fieldData field of
        SQLText text -> pure $ read $ T.unpack text -- FIXME what if db is corrupted?
        -- TODO anything else is a parse failure
