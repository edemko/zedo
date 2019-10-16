module Distribution.Zedo.Db
    ( Db
    , runDbT
    , createDb
    , resetDb
    , recordFile
    , recordDependency
    , resetDependencies
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class

import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField

import Distribution.Zedo.Data


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
