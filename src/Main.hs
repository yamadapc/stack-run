module Main
  where

import           Data.List
import           Data.List.Utils
import           Data.Maybe
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           System.Directory
import           System.Directory.ProjectRoot
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process

usage :: String
usage = unlines [ ""
                , "  Usage: stack run [name]"
                , ""
                , "  Commands:"
                , ""
                , "    stack run [name]             Compiles and runs the executable specified or the default"
                , "    stack run set-default <name> Sets the default executable to run"
                , "    stack run -- <name>          Like stack run, but will never match a sub-command"
                , ""
                ]

setDefault :: String -> IO ()
setDefault name = do
    pr <- fromMaybe (error "No project root found") <$>
        getProjectRootCurrent
    writeFile (pr </> ".stack-work" </> ".stack-run-default") name

findDefault :: IO String
findDefault = do
    pr <- fromMaybe (error "No project root found") <$>
        getProjectRootCurrent
    e <- doesFileExist (pr </> ".stack-work" </> ".stack-run-default")
    if e then readFile (pr </> ".stack-work" </> ".stack-run-default")
         else findDefault' pr
  where
    findDefault' pr = do
        cfp <- fromMaybe (error "No cabal file found") <$>
            (find ((== ".cabal") . takeExtension) <$> getDirectoryContents pr)
        getPackageDescription cfp >>= getDefaultExecutable
          where
            getPackageDescription p = parsePackageDescription <$> readFile p
            getDefaultExecutable (ParseFailed _) =
                error "Failed to parse cabal file"
            getDefaultExecutable (ParseOk _ gpd) = case condExecutables gpd of
                [] -> error "No executable found"
                ((d, _):_) -> return d

stackRun name as = do
    ph <- runCommand "stack build"
    ec <- waitForProcess ph
    case ec of
        ExitSuccess ->
            runCommand ("stack exec " ++ name ++ " -- " ++ (join " " as)) >>=
            waitForProcess >>=
            exitWith
        f -> exitWith f

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("set-default":name:_) -> setDefault name
        ("set-default":_) -> do
            hPutStrLn stderr "Missing required parameter: <name>"
            exitFailure
        ("get-default":_) -> putStrLn =<< findDefault
        ("help":_) -> do
            putStrLn usage
            exitSuccess
        ("--":name:as) -> stackRun name as
        (name:as) -> stackRun name as
        [] -> flip stackRun [] =<< findDefault
