import Zedo.Options
import Zedo.Find
import Zedo.Command

import System.Environment
import System.FilePath
import System.Directory
import System.Path
import System.Exit
import System.IO.Temp

import Control.Arrow
import Control.Monad


main :: IO ()
main = do
    putStrLn ""
    -- lookupEnv "PATH" >>= print
    lookupEnv "DEBUG" >>= \case
        Nothing -> withTempDirectory "." "testbenches.tmp" body
        Just _ -> createTempDirectory "." "testbenches.tmp" >>= body
    putStrLn "WARNING Most of test suite not yet implemented!"
    where
    body tmp = do
        tmp <- makeAbsolute tmp
        copyDir "testbenches" tmp
        test_init (tmp </> "init")
        test_find (tmp </> "find")
        test_always (tmp </> "always")


test_init tmp = do
    let topOpts = TopOptions { zedoDir = Just tmp, parent = Nothing }
    topDirs <- findTopDirs topOpts
    topDirs <- maybe (error "bad options") pure topDirs
    cmdInit topDirs
    versionStr <- readFile (tmp </> ".zedo" </> ".version")
    unlessM (doesDirectoryExist $ tmp </> ".zedo") $ die "no .zedo created"
    unless (versionStr == "zedo+sqlite v0.0.0") $ die ("wrong version found: " ++ versionStr)
    unlessM (doesFileExist $ tmp </> ".zedo" </> "db.sqlite3") $ die "no database created"
    -- TODO test the schema
    -- TODO test re-init (schema is empty even after something else messed it up)
    -- TODO test that re-init doesn't destroy the dir while another zedo process is using it

test_find tmp = do
    test_pure_find_one
    test_pure_find_twoRel
    test_pure_find_twoAbs
    where
    test_pure_find_one = do
        let topOpts = TopOptions { zedoDir = Just tmp, parent = Nothing }
        topDirs <- findTopDirs topOpts
        topDirs <- maybe (error "bad options") pure topDirs
        cmdInit topDirs
        let findOpts = TargetOptions { targetSpecifier = "one.greet" }
        targetFiles <- findTargetFiles topDirs findOpts
        let expect = TargetFiles
                { target = "one.greet"
                , targetFile = outFile expect
                , srcFile = "src/one.greet"
                , isSource = False
                , allDoFiles = [ ("do/one.greet.do", Nothing), ("do/default.greet.do", Just ".greet") ]
                , doFile = Just ("do/default.greet.do", Just ".greet")
                , otherDoFiles = [ ("do/one.greet.do", Nothing) ]
                , outFile = ".zedo/build/one.greet"
                , distFile = "dist/one.greet"
                }
        unless (checkTargetFiles expect targetFiles) $ error (unlines
            [ concat ["wrong target files found for: ", target expect]
            , concat ["  expect: ", show expect]
            , concat ["  found:  ", show targetFiles]
            ])
    test_pure_find_twoRel = do
        let topOpts = TopOptions { zedoDir = Just tmp, parent = Just "subdir/bob.greet" }
        topDirs <- findTopDirs topOpts
        topDirs <- maybe (error "bad options") pure topDirs
        cmdInit topDirs
        let findOpts = TargetOptions { targetSpecifier = "../alice.greet" }
        targetFiles <- findTargetFiles topDirs findOpts
        let expect = TargetFiles
                { target = "../alice.greet"
                , targetFile = outFile expect
                , isSource = False
                , srcFile = "src/alice.greet"
                , allDoFiles = [ ("do/alice.greet.do", Nothing), ("do/default.greet.do", Just ".greet") ]
                , doFile = Just ("do/default.greet.do", Just ".greet")
                , otherDoFiles = [ ("do/alice.greet.do", Nothing) ]
                , outFile = ".zedo/build/alice.greet"
                , distFile = "dist/alice.greet"
                }
        unless (checkTargetFiles expect targetFiles) $ error (unlines
            [ concat ["wrong target files found for: ", target expect]
            , concat ["  expect: ", show expect]
            , concat ["  found:  ", show targetFiles]
            ])
    test_pure_find_twoAbs = do
        let topOpts = TopOptions { zedoDir = Just tmp, parent = Just "subdir/bob.greet" }
        topDirs <- findTopDirs topOpts
        topDirs <- maybe (error "bad options") pure topDirs
        cmdInit topDirs
        let findOpts = TargetOptions { targetSpecifier = "/alice.greet" }
        targetFiles <- findTargetFiles topDirs findOpts
        let expect = TargetFiles
                { target = "../alice.greet"
                , targetFile = outFile expect
                , isSource = False
                , srcFile = "src/alice.greet"
                , allDoFiles = [ ("do/alice.greet.do", Nothing), ("do/default.greet.do", Just ".greet") ]
                , doFile = Just ("do/default.greet.do", Just ".greet")
                , otherDoFiles = [ ("do/alice.greet.do", Nothing) ]
                , outFile = ".zedo/build/alice.greet"
                , distFile = "dist/alice.greet"
                }
        unless (checkTargetFiles expect targetFiles) $ error (unlines
            [ concat ["wrong target files found for: ", target expect]
            , concat ["  expect: ", show expect]
            , concat ["  found:  ", show targetFiles]
            ])
    checkTargetFiles _ Nothing = False
    checkTargetFiles expected (Just targetFiles) = and
        [ targetFile expected == targetFile targetFiles `makeRelativeTo` tmp
        , srcFile expected == srcFile targetFiles `makeRelativeTo` tmp
        , isSource expected == isSource targetFiles
        , allDoFiles expected == (first (makeRelative tmp) <$> allDoFiles targetFiles)
        , doFile expected == (first (makeRelative tmp) <$> doFile targetFiles)
        , otherDoFiles expected == (first (makeRelative tmp) <$> otherDoFiles targetFiles)
        , outFile expected == outFile targetFiles `makeRelativeTo` tmp
        , distFile expected == distFile targetFiles `makeRelativeTo` tmp
        ]

test_always tmp = do
    let topOpts = TopOptions { zedoDir = Just tmp, parent = Nothing }
    topDirs <- findTopDirs topOpts
    topDirs <- maybe (error "bad options") pure topDirs
    cmdInit topDirs
    cmdAlways topDirs TargetOptions{ targetSpecifier = "one.txt" }
    whenM (doesPathExist $ tmp </> ".zedo" </> "build" </> "one.txt") $ error (concat ["file written despite targeting a source file: ", "one.txt"])
    let build name expected = do
            let targetOpts = TargetOptions { targetSpecifier = name }
                outFile = tmp </> ".zedo" </> "build" </> name
            cmdAlways topDirs targetOpts
            unlessM (doesFileExist outFile) $ error (concat ["no output file produced: ", outFile])
            contents <- readFile outFile
            unless (contents == expected) $ error (unlines [ "Unexpected file contents. Actual contents as follows:", contents ])
    build "one.greet" "Hello, World!\n"


whenM p k = p >>= flip when k
unlessM p k = p >>= flip unless k