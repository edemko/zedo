module Distribution.Zedo.Commands
    ( zedo_init
    , zedo_always
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class

import System.Directory
import System.FilePath

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

zedo_reset ::
    ( MonadReader TreeInvariants m
    , MonadIO m
    ) => m ()
zedo_reset = runDbT resetDb

zedo_always ::
    ( MonadReader TreeInvariants m
    , MonadReader InvocationInvariants m
    , MonadReader AtomInvariants m
    , MonadIO m
    ) => m ()
zedo_always = do
    target <- asks target
    -- TODO set status as running; if status is running, then spinlock...?

    ScriptSpec{..} <- findScript target
    targetType <- case scriptPath of
        Just _ -> pure Artifact
        Nothing -> doesSourceExist target >>= \case
            True -> pure Source
            False -> error "TODO create a decent error here"

    invoker <- asks invoker
    runDbT $ do
        recordFile targetType target
        resetDependencies target
        forM_ invoker $ \parent ->
            recordDependency Always parent (Right target)
        forM_ notScriptPaths $ \script ->
            recordDependency IfCreate target (Left script)
        forM_ scriptPath $ \script ->
            recordDependency IfChange target (Left script)

    case targetType of
        Source -> error "TODO unimplemented"
            -- copy source to build dir
        Artifact -> error "TODO unimplemented"
            -- run script
    -- record status
    -- record hash


findScript :: (MonadReader TreeInvariants m, MonadIO m) => TargetPath -> m ScriptSpec
findScript target = go (candidateScripts target)
    where
    go [] = error "programmer error"
    go (script@ScriptSpec{..}:rest) = do
        exists <- maybe (pure False) doesScriptExist scriptPath
        if exists then pure script else go rest


doesSourceExist :: (MonadReader TreeInvariants m, MonadIO m) => TargetPath -> m Bool
doesSourceExist (TargetPath sourcePath) = do
    sourceDir <- asks sourceDir
    liftIO $ doesFileExist (sourceDir </> sourcePath)

doesScriptExist :: (MonadReader TreeInvariants m, MonadIO m) => ScriptPath -> m Bool
doesScriptExist (ScriptPath scriptPath) = do
    scriptDir <- asks scriptDir
    liftIO $ doesFileExist (scriptDir </> scriptPath)
