module Distribution.Zedo.AgreeOn where

import Data.Functor
import Control.Monad.IO.Class

import System.FilePath
import System.Directory


invokerVar = "ZEDO__INVOKER"

baseDirVar = "ZEDO__baseDir"

verbosityVar = "ZEDO__verbosity"


findWorkDir :: FilePath -> FilePath
findWorkDir = (</> ".zedo")

findDbFile :: FilePath -> FilePath
findDbFile = findWorkDir .> (</> "db.sqlite")


findBaseDir :: MonadIO m => m (Maybe FilePath)
findBaseDir = do
    pwd <- liftIO getCurrentDirectory
    let candidates = takeUntilFixedBy equalFilePath $ iterate takeDirectory pwd
    loop candidates
    where
    loop :: MonadIO m => [FilePath] -> m (Maybe FilePath)
    loop [] = pure Nothing
    loop (candidate:rest) = checkBaseDir candidate >>= \case
        Nothing -> loop rest
        result -> pure result

checkBaseDir :: MonadIO m => FilePath -> m (Maybe FilePath)
checkBaseDir candidate = liftIO $ do
    result <- normalise <$> makeAbsolute candidate
    -- FIXME normalise doesn't remove trailing slash
    doesDirectoryExist (result </> ".zedo") <&> \case
        True -> Just result
        False -> Nothing





takeUntilFixed :: Eq a => [a] -> [a]
takeUntilFixed = takeUntilFixedBy (==)
takeUntilFixedBy :: (a -> a -> Bool) -> [a] -> [a]
takeUntilFixedBy eq [] = []
takeUntilFixedBy eq [a] = [a]
takeUntilFixedBy eq (a:b:rest)
    | a `eq` b = [a]
    | otherwise = a : takeUntilFixedBy eq (b:rest)

(.>) = flip (.)