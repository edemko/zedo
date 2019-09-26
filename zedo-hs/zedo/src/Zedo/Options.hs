{-#LANGUAGE ApplicativeDo #-}
module Zedo.Options
    ( module Options.Applicative
    , Parent
    , Command(..)
    , topCommands, subCommands
    , TopOptions(..), topOptions
    , TargetOptions(..)
    ) where

import Options.Applicative
import Data.Semigroup
import System.FilePath


type Parent = Maybe FilePath

data TopOptions = TopOptions
    { zedoDir :: Maybe FilePath
    , parent :: Parent
    -- TODO verbosity
    }
    deriving (Read, Show)

data TargetOptions = TargetOptions
    { targetSpecifier :: FilePath
    }
    deriving (Read, Show)


data Command
    = Init
    | Find TargetOptions
    | Always { targets :: [TargetOptions], find :: Bool }
    | IfChange { targets :: [TargetOptions], find :: Bool }
    | Phony
    -- TODO
    deriving (Read, Show)

topOptions = do
    let parent = Nothing
    zedoDir <- optional (strOption
                ( long "zedo-dir"
                <> short 'd'
                <> help "Set working directory explicitly.\nOtherwise, it is the first parent directory containing a `.zedo` directory."
                ))
    pure TopOptions{..}

(topCommands, subCommands) =
    let basic =
            (  command "find"     (info findOptions
                    (progDesc "Report information about a zedo target."))
            <> command "always"   (info alwaysOptions
                    (progDesc "Rebuild a target, regardless of the state of its dependencies."))
            <> command "ifchange" (info ifchangeOptions
                    (progDesc "Rebuild a target, but only if its dependencies are out-of-date."))
            )
        init = command "init" (info initOptions
                    (progDesc "Create a zedo project."))
        sub =
            (  command "phony" (info phonyOptions
                    (progDesc "Do not produce an output file for the calling target."))
            -- TODO volatile
            )
        def = defaultOptions
    in (hsubparser (init <> basic) <|> def, hsubparser (basic <> sub) <|> def)


initOptions = pure Init
findOptions = do
    targetSpecifier <- argument str (metavar "TARGET")
    pure $ Find TargetOptions{..}
alwaysOptions = do
    find <- switch (  long "find"
                   <> short 'f'
                   <> help "Also print the target filename.")
    targets <- targetsOptions
    pure Always{..}
ifchangeOptions = do
    find <- switch (  long "find"
                   <> short 'f'
                   <> help "Also print the target filename.")
    targets <- targetsOptions
    pure IfChange{..}
phonyOptions = pure Phony
defaultOptions = do
    find <- pure False
    targets <- targetsOptions
    pure Always{..}

targetsOptions = some $ do
    targetSpecifier <- argument str (metavar "TARGET")
    pure TargetOptions{..}
