import Zedo.Options
import Zedo.Find
import Zedo.Command
import Zedo.Db

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
    withCurrentDirectory (Zedo.Find.zedoDir topDirs) $ do
        cmdInit topDirs
        versionStr <- readFile (tmp </> ".zedo" </> ".version")
        unlessM (doesDirectoryExist $ tmp </> ".zedo") $ die "no .zedo created"
        unless (versionStr == "zedo+sqlite v0.0.0") $ die ("wrong version found: " ++ versionStr)
        unlessM (doesFileExist $ tmp </> ".zedo" </> "db.sqlite3") $ die "no database created"
        -- TODO test the schema
        -- TODO test re-init (schema is empty even after something else messed it up)
        -- TODO test that re-init doesn't destroy the dir while another zedo process is using it

test_find tmp = do
    let topOpts = TopOptions { zedoDir = Just tmp, parent = Nothing }
    topDirs <- findTopDirs topOpts
    topDirs <- maybe (error "bad options") pure topDirs
    withCurrentDirectory (Zedo.Find.zedoDir topDirs) $ do
        cmdInit topDirs
        test_pure_find_one
        test_pure_find_twoRel
        test_pure_find_twoAbs
    where
    test_pure_find_one = do
        let topOpts = TopOptions { zedoDir = Just tmp, parent = Nothing }
        topDirs <- findTopDirs topOpts
        topDirs <- maybe (error "bad options") pure topDirs
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
        [ targetFile expected == targetFile targetFiles
        , srcFile expected == srcFile targetFiles
        , isSource expected == isSource targetFiles
        , allDoFiles expected == allDoFiles targetFiles
        , doFile expected == doFile targetFiles
        , otherDoFiles expected == otherDoFiles targetFiles
        , outFile expected == outFile targetFiles
        , distFile expected == distFile targetFiles
        ]

test_always tmp = do
    let topOpts = TopOptions { zedoDir = Just tmp, parent = Nothing }
    topDirs <- findTopDirs topOpts
    topDirs <- maybe (error "bad options") pure topDirs
    withCurrentDirectory (Zedo.Find.zedoDir topDirs) $ do
        cmdInit topDirs
        cmdAlways topDirs TargetOptions{ targetSpecifier = "one.txt" }
        whenM (doesPathExist $ tmp </> ".zedo" </> "build" </> "one.txt") $ error (concat ["file written despite targeting a source file: ", "one.txt"])
        let build name expected = do
                let targetOpts = TargetOptions { targetSpecifier = name }
                    outFile = tmp </> ".zedo" </> "build" </> name
                cmdAlways topDirs targetOpts
                state <- withDb topDirs $ \db -> peekStatus db name
                case state of
                    Ok _ -> pure ()
                    _ -> error ("no 'ok' record for: " ++ name)
                unlessM (doesFileExist outFile) $ error (concat ["no output file produced: ", outFile])
                contents <- readFile outFile
                unless (contents == expected) $ error (unlines [ "Unexpected file contents. Actual contents as follows:", contents ])
            buildPhony name = do
                let targetOpts = TargetOptions { targetSpecifier = name }
                    outFile = tmp </> ".zedo" </> "build" </> name
                cmdAlways topDirs targetOpts
                state <- withDb topDirs $ \db -> peekStatus db name
                case state of
                    Ok _ -> pure ()
                    _ -> error ("no 'ok' record for: " ++ name)
                whenM (doesFileExist outFile) $ error (concat ["unexpected output file produced: ", outFile])
        build "one.greet" "Hello, World!\n"
        buildPhony "phony"


whenM p k = p >>= flip when k
unlessM p k = p >>= flip unless k