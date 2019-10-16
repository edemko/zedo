module Distribution.Zedo.Db
    ( Db
    , runDbT
    , withTreeLock
    , withAtomLock
    , createDb
    , resetDb
    , recordFile
    , recordDependency
    , resetDependencies
    ) where

import Data.Maybe

import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Concurrent

import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import qualified Database.SQLite.Simple.Ok as SQL

import Distribution.Zedo.Data


withTreeLock :: (MonadReader TreeInvariants m, MonadIO m)
                    => m a -> m a
withTreeLock action = do
    -- TODO check that no other zedo process is running and put this pid in
    -- TODO if it is running, throw an error
    -- TODO otherwise, bracket the following with a release of the lock:
    runDbT $ Db $ do
        conn <- ask
        liftIO $ execute_ conn "UPDATE file SET status = NULL;"
    action

withAtomLock :: ( MonadReader TreeInvariants m
                , MonadReader AtomInvariants m
                , MonadCatch m
                , MonadIO m)
                => a -> m a -> m a
withAtomLock def action = do
    TargetPath targetPath <- asks target
    status <- runDbT $ do
        status <- getStatus targetPath
        when (isNothing status) $ setStatus Running targetPath
        -- NOTE after this point, if any statuses get left as 'RUN', a later invocation tree will clear out all the statuses anyway
        pure status
    dispatch status bracketed
    where
    samplesPerSecond = 10
    dispatch Nothing action = action
    dispatch (Just Running) _ = spinlock
    dispatch (Just Ok) _ = pure def
    dispatch (Just Error) _ = do
        target <- asks target
        throwM $ ZedoFailure target
    bracketed = do
        TargetPath targetPath <- asks target
        let body = do
                a <- action
                runDbT $ setStatus Ok targetPath
                pure a
            handler = runDbT $ setStatus Error targetPath
        body `onException` handler

    getStatus targetPath = Db $ do
        conn <- ask
        [Only status] <- liftIO $ queryNamed conn
            "SELECT status FROM file WHERE path = :targetPath;"
            [":targetPath" := targetPath]
        pure status
    setStatus status targetPath = Db $ do
        conn <- ask
        liftIO $ executeNamed conn
            "UPDATE file SET status = :newStatus WHERE path = :targetPath;"
            [":targetPath" := targetPath, ":newStatus" := status]
    spinlock = do
        liftIO $ threadDelay (1000000 `div` samplesPerSecond)
        TargetPath targetPath <- asks target
        status <- runDbT (getStatus targetPath)
        dispatch status $ withAtomLock def action

data TargetStatus
    = Ok
    | Error
    | Running





newtype Db a = Db { unDb :: ReaderT Connection IO a }
    deriving (Functor, Applicative, Monad)

runDbT :: (MonadReader TreeInvariants m, MonadIO m) => Db a -> m a
runDbT action = do
    dbFile <- asks dbFile
    liftIO $ withConnection dbFile $ \conn ->
        withTransaction conn $ do
            execute_ conn "PRAGMA foreign_keys=1;"
            flip runReaderT conn $
                unDb action





createDb :: Db ()
createDb = Db $ do
    conn <- ask
    liftIO $ execute_ conn fileTable
    liftIO $ execute_ conn depTable
    where
    fileTable =
        "CREATE TABLE IF NOT EXISTS file\n\
        \    ( type              TEXT NOT NULL\n\
        \    , path              TEXT PRIMARY KEY\n\
        \    , last_known_hash   TEXT\n\
        \    , isPhony           BOOL NOT NULL DEFAULT(0)\n\
        \\n\
        \    , CONSTRAINT enum_file_type CHECK (type IN\n\
        \        ('SOURCE', 'ARTIFACT', 'SCRIPT', 'PATTERN', 'EXTERNAL')\n\
        \    )\n\
        \);"
    depTable =
        "CREATE TABLE IF NOT EXISTS dep\n\
        \    ( target        TEXT NOT NULL REFERENCES file(path)\n\
        \    , dependency    TEXT NOT NULL REFERENCES file(path)\n\
        \    , type          TEXT NOT NULL\n\
        \\n\
        \    , CONSTRAINT dep_pk PRIMARY KEY (parent, child)\n\
        \    , CONSTRAINT enum_dep_type CHECK (type IN\n\
        \        ('ALWAYS', 'CHANGE', 'CREATE')\n\
        \    )\n\
        \);"

resetDb :: Db ()
resetDb = Db $ do
    conn <- ask
    liftIO $ execute_ conn cleanDep
    liftIO $ execute_ conn cleanFile
    where
    cleanFile = "DELETE FROM file;"
    cleanDep = "DELETE FROM dep;"


recordFile :: TargetType -> TargetPath -> Db ()
recordFile targetType targetPath = Db $ do
    conn <- ask
    liftIO $ executeNamed conn
        "INSERT OR IGNORE INTO file (path) VALUES (:targetPath);"
        [":targetPath" := targetPath]
    -- TODO if status is not null/running, fail if the type is unequal
    liftIO $ executeNamed conn
        "UPDATE file SET type = :targetType WHERE path = :targetPath;"
        [":targetType" := targetType, ":targetPath" := targetPath]

recordDependency :: DependencyType -> TargetPath -> (Either ScriptPath TargetPath) -> Db ()
recordDependency depType target dependency = Db $ do
    conn <- ask
    let depPath = case dependency of
            Left (ScriptPath path) -> path
            Right (TargetPath path) -> path
    liftIO $ executeNamed conn
        "INSERT INTO dep (target, dependency, type)\n\
        \VALUES (:target, :depPath, :depType);"
        [":target" := target, ":depPath" := depPath, ":depType" := depType]

resetDependencies :: TargetPath -> Db ()
resetDependencies (TargetPath targetPath) = Db $ do
    conn <- ask
    liftIO $ executeNamed conn
        "DELETE FROM dep WHERE target = :targetPath;"
        [":targetPath" := targetPath]





instance ToField TargetType where
    toField Source = SQLText "SOURCE"
    toField Artifact = SQLText "ARTIFACT"
    toField Script = SQLText "SCRIPT"
    toField External = SQLText "EXTERNAL"
    toField Pattern = SQLText "PATTERN"

instance ToField TargetPath where
    toField (TargetPath path) = SQLText (T.pack path)

instance ToField DependencyType where
    toField Always = SQLText "ALWAYS"
    toField IfChange = SQLText "CHANGE"
    toField IfCreate = SQLText "CREATE"

instance ToField TargetStatus where
    toField Ok = SQLText "OK"
    toField Error = SQLText "ERR"
    toField Running = SQLText "RUN"
instance FromField TargetStatus where
    fromField field = case fieldData field of
        SQLText "OK"  -> SQL.Ok Ok
        SQLText "ERR" -> SQL.Ok Error
        SQLText "RUN" -> SQL.Ok Running
