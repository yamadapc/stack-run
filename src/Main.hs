{-# LANGUAGE CPP #-}
module Main
  where

import           Control.Applicative                   ((*>), (<$>))
import           Control.Concurrent.Async
import           Control.Monad                         (unless)
import qualified Data.ByteString                       as ByteString
import qualified Data.ByteString.Char8                 as ByteString8
import           Data.Conduit
import qualified Data.Conduit.Binary                   as Conduit.Binary
import qualified Data.Conduit.List                     as Conduit.List
import           Data.Conduit.Process
import           Data.List
import           Data.List.Utils
import           Data.Maybe
import           Data.Time
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parsec
import           Distribution.Types.UnqualComponentName
import           System.Console.ANSI
#ifndef OS_Win32
import           System.Console.Questioner
#endif
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Error

usage :: String
usage = unlines [ ""
                , "  Usage: stack run <name|sub-command> [args]"
                , ""
                , "  Commands:"
                , ""
                , "    stack run [name] [args]      Compiles and runs the executable specified or the default"
                , "    stack run -- <name> [args]   Like stack run, but will never match a sub-command"
                , "    stack run -- -- [args]       Pass-in arguments to the default executable"
                , ""
                , "    stack run set-default <name> Sets the default executable to run"
                , ""
                , "    stack run --interactive      Runs in interactive mode"
                , "    stack run -i"
                , ""
                , "    stack run help               Print this help message"
                , "    stack run --help"
                , "    stack run -h"
                , ""
                ]

setDefault :: String -> IO ()
setDefault name = do
    pr <- fromMaybe (error "No project root found") <$> getCabalProjectRootCurrent
    writeFile (pr </> ".stack-work" </> ".stack-run-default") name

findDefault :: IO String
findDefault = do
    pr <- fromMaybe (error "No project root found") <$> getCabalProjectRootCurrent
    e <- doesFileExist (pr </> ".stack-work" </> ".stack-run-default")
    if e then readFile (pr </> ".stack-work" </> ".stack-run-default")
         else findDefault' pr
  where
    findDefault' :: FilePath -> IO String
    findDefault' pr = do
        cfp <- fromMaybe (error "No cabal file found") <$> (find ((== ".cabal") . takeExtension) <$> getDirectoryContents pr)
        (_, result) <- runParseResult . parseGenericPackageDescription <$> ByteString.readFile (pr </> cfp)
        case result of
          Left _ -> error "No executable found"
          Right gpd -> case condExecutables gpd of
                         [] -> error "No executable found"
                         ((d, _):_) -> pure $ unUnqualComponentName d

getExecutables :: IO [String]
getExecutables = do
    pr <- fromMaybe (error "No project root found") <$> getCabalProjectRootCurrent
    cfp <- fromMaybe (error "No cabal file found") <$> (find ((== ".cabal") . takeExtension) <$> getDirectoryContents pr)

    (_, result) <- runParseResult . parseGenericPackageDescription <$> ByteString.readFile (pr </> cfp)
    case result of
      Left _ -> error "Failed to parse cabal file"
      Right gpd -> case condExecutables gpd of
                     [] -> error "No executable found"
                     ds -> pure $ map (unUnqualComponentName . fst) ds

getCabalProjectRootCurrent :: IO (Maybe FilePath)
getCabalProjectRootCurrent = flip catchIOError (const (pure Nothing)) $
  getCurrentDirectory >>= getCabalProjectRoot
  where
    getCabalProjectRoot :: FilePath -> IO (Maybe FilePath)
    getCabalProjectRoot path = do
      hasCabal <- any (isSuffixOf ".cabal") <$> getDirectoryContents path
      if hasCabal
        then pure $ Just path
        else let parent = takeDirectory path in
          if parent /= path
            then getCabalProjectRoot parent
            else pure Nothing

stackRun :: String -> [String] -> IO b
stackRun name as = do
    pr <- fromMaybe (error "No project root found") <$> getCabalProjectRootCurrent
    stackYmlExists <- doesFileExist (pr </> "stack.yaml")
    unless stackYmlExists $ do
        ec <- prettyRunCommand "stack init"
        case ec of
            ExitSuccess -> pure ()
            f -> exitWith f
    start <- getCurrentTime
    ec <- prettyRunCommand "stack build"
    hSetSGR stderr [SetColor Foreground Vivid Black]
    fdiff <- getDiffTime start
    hPutStrLn stderr ("stack build took " ++ show fdiff ++ "ms")
    hSetSGR stderr [Reset]
    case ec of
        ExitSuccess -> do
            let cmd = "stack exec " ++ name ++ " -- " ++ join " " as
            logCommand cmd
            hSetSGR stderr [Reset]
            hFlush stderr
            ph <- runCommand cmd
            ec' <- waitForProcess ph
            exitWith ec'
        f -> exitWith f
  where
    getDiffTime :: UTCTime -> IO Integer
    getDiffTime start = do
        now <- getCurrentTime
        let diff :: Double
            diff = fromRational (toRational (diffUTCTime now start))
            fdiff = floor (diff * 1000)
        pure fdiff


prettyRunCommand :: String -> IO ExitCode
prettyRunCommand cmd = do
    hSetBuffering stderr LineBuffering
    logCommand cmd
    (Inherited, out, err, cph) <- streamingProcess (shell cmd)
    runConcurrently $
        Concurrently (out $$ (Conduit.Binary.lines .| Conduit.List.mapM_ putLineGray)) *>
        Concurrently (err $$ (Conduit.Binary.lines .| Conduit.List.mapM_ putLineRed)) *>
        Concurrently (waitForStreamingProcess cph)
  where
    putLineSGR sgr b = do
        hSetSGR stderr sgr
        hPutStr stderr "  "
        ByteString8.hPutStrLn stderr b
        hSetSGR stderr [Reset]
    putLineGray = putLineSGR [SetColor Foreground Dull White]
    putLineRed = putLineSGR [SetColor Foreground Vivid Black]

logCommand :: String -> IO ()
logCommand cmd = do
    hSetSGR stderr [ SetColor Foreground Vivid White
                   , SetConsoleIntensity BoldIntensity
                   ]
    hPutStr stderr "$ "
    hSetSGR stderr [Reset]
    hSetSGR stderr [SetColor Foreground Vivid Cyan]
    hPutStrLn stderr cmd
    hSetSGR stderr [Reset]

#ifndef OS_Win32
runInteractive :: [String] -> IO ()
runInteractive as = do
    exs <- getExecutables
    ex <- prompt ("What executable should we run? ", exs)
    stackRun ex as
#endif

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
#ifndef OS_Win32
        ("-i":as) -> runInteractive as
        ("--interactive":as) -> runInteractive as
#endif
        ("help":_) -> exitUsage
        ("--":"--":as) -> flip stackRun as =<< findDefault
        ("--":name:as) -> stackRun name as
        (name:as) -> stackRun name as
        [] -> flip stackRun [] =<< findDefault
  where
    exitUsage = do
        putStr usage
        exitSuccess
