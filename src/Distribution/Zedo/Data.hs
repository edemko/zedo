module Distribution.Zedo.Data where


newtype TargetPath = TargetPath { unTargetPath :: FilePath }
    deriving (Show)
newtype ScriptPath = ScriptPath { unScriptPath :: FilePath }
    deriving (Show)

data TargetType
    = Source
    | Artifact
    | Script
    | External
    | Pattern

data DependencyType
    = Always
    | IfChange
    | IfCreate

data ScriptSpec = ScriptSpec
    { scriptPath :: Maybe ScriptPath
    , scriptExtension :: Maybe String
    , notScriptPaths :: [ScriptPath]
    }


data TreeInvariants = TI
    { baseDir :: FilePath
    , workDir :: FilePath -- these are derived
    , dbFile :: FilePath -- these are derived
    -- , sourceDir :: FilePath
    -- , buildDir :: FilePath
    -- , scriptDir :: FilePath
    -- , hashAlg :: ??? -- TODO

    , verbosity :: Int -- FIXME use an enum
    }

data InvocationInvariants = II
    { invoker :: Maybe TargetPath
    , isRoot :: Bool -- derived
    }

data AtomInvariants = AI
    { target :: TargetPath
    }
