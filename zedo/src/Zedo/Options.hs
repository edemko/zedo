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
    | Always [TargetOptions]
    | IfChange [TargetOptions]
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
            (  command "find" (info findOptions (progDesc "Report information about a zedo target or source file."))
            <> command "always" (info (Always <$> targetsOptions) (progDesc "Rebuild a target, regardless of the state of its dependencies."))
            <> command "ifchange" (info (IfChange <$> targetsOptions) (progDesc "Rebuild a target, but only if its dependencies are out-of-date."))
            )
        init = command "init" (info initOptions (progDesc "Create a zedo project."))
        def = (Always <$> targetsOptions)
    in (hsubparser (init <> basic) <|> def, hsubparser basic <|> def)


initOptions = pure Init
findOptions = do
    targetSpecifier <- argument str (metavar "TARGET")
    pure $ Find TargetOptions{..}
targetsOptions = some $ do
    targetSpecifier <- argument str (metavar "TARGET")
    pure TargetOptions{..}
