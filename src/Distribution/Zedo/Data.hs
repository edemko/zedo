module Distribution.Zedo.Data where


newtype TargetPath = TargetPath FilePath

data TargetType
    = Source
    | Artifact
    | Script
    | External
    | Pattern

data ScriptSpec = ScriptSpec
    { scriptPath :: FilePath -- relative to script directory...?
    , scriptExtension :: Maybe String
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
