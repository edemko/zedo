module Distribution.Zedo.Db
    ( Db
    , runDbT
    , createDb
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class

import Database.SQLite.Simple
import Distribution.Zedo.Data


newtype Db a = Db { unDb :: ReaderT Connection IO a }

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
        \        ('SOURCE', 'BUILD', 'SCRIPT', 'PATTERN', 'EXTERNAL')\n\
        \    )\n\
        \);"
    depTable =
        "CREATE TABLE IF NOT EXISTS dep\n\
        \    ( parent    TEXT NOT NULL REFERENCES file(path)\n\
        \    , child     TEXT NOT NULL REFERENCES file(path)\n\
        \    , type      TEXT NOT NULL\n\
        \\n\
        \    , CONSTRAINT enum_dep_type CHECK (type IN\n\
        \        ('ALWAYS', 'CHANGE', 'CREATE')\n\
        \    )\n\
        \);\n\
        \"
