module Main where

-- data structures
import Data.Function
import Data.Maybe
import Data.Semigroup hiding (option)

-- control flow
import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class

-- side-effects
import System.IO
import System.Environment
import System.Directory
import System.Exit

-- data types
import System.FilePath

-- internal dependencies
import Distribution.Zedo.Data
import Distribution.Zedo.AgreeOn
import OptParse
import Distribution.Zedo.Commands
import Distribution.Zedo.ReaderStackHack


main :: IO ()
main = do
    inv <- getInvocationInvariants
    flip runReaderT inv $ do
        (tree, command) <- asks isRoot >>= \case
            True -> do
                args <- getRootArguments
                tree <- setupTreeInvariants args
                -- TODO export tree invariants
                pure (tree, theCommand args)
            False -> do
                tree <- loadTreeInvariants
                args <- getChildArguments tree
                pure (tree, theCommand args)
        flip runReaderT tree $ do
            (liftIO . print) command
            (liftIO . print) =<< asks baseDir
            case command of
                Init _ -> zedo_init
                _ -> error "TODO unimplemented"

getInvocationInvariants :: MonadIO m => m InvocationInvariants
getInvocationInvariants = do
    invoker <- (TargetPath <$>) <$> getZedoEnv invokerVar
    let isRoot = isNothing invoker
    pure II{..}





getZedoEnv :: (MonadIO m) => String -> m (Maybe String)
getZedoEnv var = liftIO $ lookupEnv var <&> \case
    Just "" -> Nothing
    x -> x

setupTreeInvariants :: MonadIO m => Arguments -> m TreeInvariants
setupTreeInvariants Args{..} = do
    baseDir <- case theCommand of
        Init Nothing -> liftIO getCurrentDirectory
        Init (Just dir) -> pure dir
        _ -> do
            baseDir <- case zedoDir of
                Nothing -> findBaseDir
                Just zedoDir -> checkBaseDir zedoDir
            maybe noBaseDirErr pure baseDir
    let workDir = findWorkDir baseDir
        dbFile = findDbFile baseDir
        sourceDir = baseDir </> "src" -- TODO
        artifactDir = baseDir </> "build" -- TODO
        scriptDir = baseDir </> "do" -- TODO
    pure TI{..}
    where
    noBaseDirErr = liftIO $ do
        putErrLn "Not a zedo project (or any of the parent directories)"
        exitFailure
loadTreeInvariants :: MonadIO m => m TreeInvariants
loadTreeInvariants = do
    baseDir <- fromJust <$> getZedoEnv baseDirVar -- FIXME lots of assumptions made here
    verbosity <- read . fromJust <$> getZedoEnv verbosityVar -- FIXME lots of assumptions made here
    let isRoot = False
        workDir = findWorkDir baseDir
        dbFile = findDbFile baseDir
        sourceDir = baseDir </> "src" -- TODO
        artifactDir = baseDir </> "build" -- TODO
        scriptDir = baseDir </> "do" -- TODO
    pure TI{..}

putErrLn = hPutStrLn stderr
