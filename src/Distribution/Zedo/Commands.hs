module Distribution.Zedo.Commands
    ( zedo_init
    , zedo_always
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class

import System.Directory

import Distribution.Zedo.AgreeOn
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

zedo_always ::
    ( MonadReader TreeInvariants m
    , MonadReader InvocationInvariants m
    , MonadReader AtomInvariants m
    , MonadIO m
    ) => m ()
zedo_always = do
    target <- asks target
    script <- findScript target
    invoker <- asks invoker
    runDbT $ do
        -- record this file
        case invoker of
            Nothing -> pure ()
            Just parent -> recordDependency Always parent target
        -- record dependencies of this file on scripts
    -- if source file:
    --      copy source to build dir
    -- else target file
    --      runs script
    -- record status
    -- record hash

findScript :: MonadIO m => TargetPath -> m ScriptSpec
findScript target = liftIO $ go (candidateScripts target)
    where
    go [] = error "programmer error"
    go (script@ScriptSpec{..}:rest) = do
        exists <- maybe (pure False) (doesFileExist . unScriptPath) scriptPath
        if exists then pure script else go rest
