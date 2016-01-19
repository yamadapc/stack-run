module Main
  where

import           Control.Applicative                   ((<$>))
import           Control.Monad                         (unless)
import qualified Data.ByteString.Char8                 as ByteString
import           Data.Conduit
import qualified Data.Conduit.Binary                   as Conduit.Binary
import qualified Data.Conduit.List                     as Conduit.List
import           Data.Conduit.Process
import           Data.List
import           Data.List.Utils
import           Data.Maybe
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           System.Console.ANSI
import           System.Directory
import           System.Directory.ProjectRoot
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO

usage :: String
usage = unlines [ ""
                , "  Usage: stack run <name|sub-command> [args]"
                , ""
                , "  Commands:"
                , ""
                , "    stack run [name] [args]      Compiles and runs the executable specified or the default"
                , "    stack run set-default <name> Sets the default executable to run"
                , "    stack run -- <name> [args]   Like stack run, but will never match a sub-command"
                , "    stack run -- -- [args]       Pass-in arguments to the default executable"
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

stackRun :: String -> [String] -> IO b
stackRun name as = do
    pr <- fromMaybe (error "No project root found") <$> getProjectRootCurrent
    stackYmlExists <- doesFileExist (pr </> "stack.yaml")
    unless stackYmlExists $ do
        ec <- prettyRunCommand "stack init"
        case ec of
            ExitSuccess -> return ()
            f -> exitWith f
    ec <- prettyRunCommand "stack build"
    case ec of
        ExitSuccess -> do
            let cmd = "stack exec " ++ name ++ " -- " ++ join " " as
            logCommand cmd
            setSGR [Reset]
            hFlush stdout
            ph <- runCommand cmd
            ec' <- waitForProcess ph
            exitWith ec'
        f -> exitWith f

prettyRunCommand :: String -> IO ExitCode
prettyRunCommand cmd = do
    logCommand cmd
    (Inherited, out, err, cph) <- streamingProcess (shell cmd)
    out =$= Conduit.Binary.lines $$ Conduit.List.mapM_ putLineGray
    err =$= Conduit.Binary.lines $$ Conduit.List.mapM_ putLineRed
    waitForStreamingProcess cph
  where
    putLineSGR sgr b = do
        setSGR sgr
        putStr "  "
        ByteString.putStrLn b
        setSGR [Reset]
    putLineGray = putLineSGR [SetColor Foreground Dull White]
    putLineRed = putLineSGR [SetColor Foreground Vivid Black]

logCommand :: String -> IO ()
logCommand cmd = do
    setSGR [ SetColor Foreground Vivid White
           , SetConsoleIntensity BoldIntensity
           ]
    putStr "$ "
    setSGR [Reset]
    setSGR [SetColor Foreground Vivid Cyan]
    putStrLn cmd
    setSGR [Reset]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("set-default":name:_) -> setDefault name
        ("set-default":_) -> do
            hPutStrLn stderr "Missing required parameter: <name>"
            exitFailure
        ("get-default":_) -> putStrLn =<< findDefault
        ("--help":_) -> exitUsage
        ("-h":_) -> exitUsage
        ("help":_) -> exitUsage
        ("--":name:as) -> stackRun name as
        (name:as) -> stackRun name as
        [] -> flip stackRun [] =<< findDefault
  where
    exitUsage = do
        putStr usage
        exitSuccess
