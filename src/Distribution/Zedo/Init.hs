module Distribution.Zedo.Init where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class

import System.Directory

import Distribution.Zedo.Data
import Distribution.Zedo.Db


zedo_init ::
    ( MonadReader TreeInvariants m
    , MonadReader InvocationInvariants m
    , MonadIO m
    ) => m ()
zedo_init = do
    liftIO . createDirectoryIfMissing False =<< asks workDir
    -- FIXME do nothing if nothing need be done
    -- TODO create default configuration files
    runDbT createDb