-- FIXME Options.Applicative lets be do autocompletion stuff
module OptParse
    ( Arguments(..)
    , Command(..)
    , getRootArguments
    , getChildArguments
    ) where

import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Options.Applicative

import Data.Version

import Paths_zedo
import Distribution.Zedo.Data (TreeInvariants(..))


getRootArguments :: MonadIO m => m Arguments
getRootArguments = getArguments Nothing

getChildArguments :: MonadIO m => TreeInvariants -> m Arguments
getChildArguments = getArguments . Just

getArguments :: MonadIO m => Maybe TreeInvariants -> m Arguments
getArguments tree = liftIO $ execParser $ info
    (maybe rootOptions childOptions tree <**> helper <**> versioner version)
    ( fullDesc
    -- <> progDesc "TODO describe what the invocation does"
    <> header "zedo - rebuild target files when source files have changed"
    )


data Arguments = Args
    { verbosity :: Int
    , zedoDir :: Maybe FilePath
    -- TODO j for how many process can be active at once

    , theCommand :: Command
    }
    deriving (Show)
data Command
    = Init (Maybe FilePath)
    | Reset
    | Always [String]
    | IfCreate [String]
    | IfChange [String]
    | Phony
    -- | Touch [String] -- uh... what exactly is touch supposed to do?
    -- Log?
    deriving (Show)

rootOptions :: Parser Arguments
rootOptions = Args
    <$> verbosityParse
    <*> zedoDirParse
    <*> commandParse -- FIXME only allow init, always, ifchange, ifcreate
childOptions :: TreeInvariants -> Parser Arguments
childOptions TI{..} = Args
    <$> pure verbosity
    <*> pure Nothing
    <*> commandParse -- FIXME disallow init

versioner :: Version -> Parser (a -> a)
versioner v = abortOption (InfoMsg $ showVersion v)
    ( long "version"
    <> help "Display version number on stdout"
    <> hidden
    )

verbosityParse = evalVerbosityLevel <$> many
    ( flag' Inc
        (  short 'v'
        <> long "verbose"
        <> help "request more informational logging"
        )
    <|> flag' Dec
        (  short 'q'
        <> long "quiet"
        <> help "request less informational logging"
        )
    <|> option (Set <$> auto)
        (  long "verbosity"
        <> help "set verbosity level"
        <> metavar "LEVEL"
        )
    )
data VerbosityAdjust = Inc | Dec | Set Int deriving (Show)
evalVerbosityLevel :: [VerbosityAdjust] -> Int
evalVerbosityLevel = ($ 1) . foldr (flip (.)) clamp . map evalPrim
    where
    evalPrim Inc x = x + 1
    evalPrim Dec x = x - 1
    evalPrim (Set v) _ = v
    clamp = min 5 . max 0

zedoDirParse = optional $ strOption
    (  long "zedo-dir"
    <> help "explicitly set the path to the project"
    <> metavar "PATH"
    <> showDefaultWith (const "nearest parent of pwd containing a .zedo directory")
    )

commandParse =
    subparser
        (  command "init"
            (info (initOpts <**> helper)
                (progDesc "Create a zedo project at DIR. Does nothing if it's already initialized.")
            )
        <> command "reset"
            (info (resetOpts <**> helper)
                (progDesc "Clean out the database in case of corruption.")
            )
        <> command "always"
            (info (alwaysOpts <**> helper)
                (progDesc $ unwords
                    [ "Execute do-script for each TARGET."
                    , "The calling script (if any) will always be re-run."
                    , "This is the default command when none is supplied."
                    , "This is only really useful at top-level, and also for certain phony targets (which themselves should be near top-level)."
                    ]
                )
            )
        <> command "ifchange" (info (alwaysOpts <**> helper)
                (progDesc $ unwords
                    [ "Execute do-script for each TARGET if its dependencies are out-of-date."
                    , "The calling script will be re-run if any of the TARGETs are changed or removed."
                    ]
                )
            )
        <> command "ifcreate" (info (alwaysOpts <**> helper)
                (progDesc $ unwords
                    [ "Ensures that each TARGET is a non-existent source file."
                    , "The calling script will be re-run if any of the TARGETs are created."
                    ]
                )
            )
        )
    <|>
    (Always <$> targetsParse)
    where
    initOpts = Init <$>
        (   argument (Just <$> str) (metavar "DIR")
        <|> pure Nothing
        )
    resetOpts = pure Reset
    alwaysOpts = Always <$> targetsParse
    ifchangeOpts = Always <$> targetsParse
    ifcreateOpts = Always <$> targetsParse

targetsParse :: Parser [String]
targetsParse = many $ argument str ( metavar "TARGETS..." )
