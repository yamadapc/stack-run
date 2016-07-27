{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
module System.Console.Questioner
    (
      Question(..)

    , ChoiceEvent
    , charToChoiceEvent
    , listPrompt
    , checkboxPrompt

    , module System.Console.Questioner.ProgressIndicators
    )
  where

import           Control.Applicative                          ((<$>))
import           Control.Concurrent.STM
import           Control.Monad                                (forM_, (>=>))
import           Data.List                                    (delete)
import           Graphics.Vty                                 (Event (..),
                                                               Key (..),
                                                               Modifier (..))
import qualified Graphics.Vty                                 as Vty
import           System.Console.ANSI                          (Color (..), ColorIntensity (..), ConsoleIntensity (..), ConsoleLayer (..),
                                                               SGR (..),
                                                               clearLine,
                                                               cursorUpLine,
                                                               setSGR)
import           System.Console.Questioner.ProgressIndicators
import           System.Console.Questioner.Util
import           System.Exit
import           System.IO                                    (hFlush, stdin,
                                                               stdout)

-- Base `Question` and `Question` instances
-------------------------------------------------------------------------------

class Question q a where
    prompt :: q -> IO a

instance {-# OVERLAPPABLE #-} Read a => Question String a where
    prompt = putStr . (++ " ") >=> const readLn

instance {-# OVERLAPPING #-} Question String String where
    prompt = putStr . (++ " ") >=> const getLine

instance {-# OVERLAPPING #-} Question String (Maybe String) where
    prompt = putStr . (++ " ") >=> const getLine >=> helper
      where
        helper [] = return Nothing
        helper s = return $ Just s

instance {-# OVERLAPPING #-} Question (String, (String, String)) String where
    prompt (s, (o1, o2)) = do
        putStr s
        putStr $ " (" ++ o1 ++ "/" ++ o2 ++ ") "
        getLine

instance {-# OVERLAPPING #-} Question (String, [String]) String where
    prompt = uncurry listPrompt

instance {-# OVERLAPPING #-} Question (String, [String]) [String] where
    prompt = uncurry checkboxPrompt

-- Multiple choice prompts
-------------------------------------------------------------------------------

data ChoiceEvent = MoveUp | MoveDown | MakeChoice | ToggleSelection | Exit
  deriving(Eq, Ord, Show)

charToChoiceEvent :: Char -> Maybe ChoiceEvent
charToChoiceEvent 'j'  = Just MoveDown
charToChoiceEvent 'k'  = Just MoveUp
charToChoiceEvent '\n' = Just MakeChoice
charToChoiceEvent ' '  = Just ToggleSelection
charToChoiceEvent _    = Nothing

-- simpleListPrompt options choices = setup $ do
--     inp <- Vty.inputForConfig =<< Vty.standardIOConfig
--     selection <- waitForSelection (Vty._eventChannel inp) 0
--     setSGR []
--     clearScreen
--     setCursorPosition 0 0
--     Vty.shutdownInput inp
--     return selection
--   where
--     setup = withNoBuffering stdin NoBuffering . withNoCursor . withNoEcho
--     numChoices = length choices

--     waitForSelection ichan currentIdx = do
--         clearScreen
--         renderListOptions options def choices currentIdx
--         e <- atomically $ readTChan ichan
--         case e of
--             EvKey KEnter _ -> return $ Just (choices !! currentIdx)
--             EvKey (KChar 'n') [MCtrl] -> onDown
--             EvKey (KChar 'j') _ -> onDown
--             EvKey KDown _ -> onDown
--             EvKey (KChar 'p') [MCtrl] -> onUp
--             EvKey (KChar 'k') _ -> onUp
--             EvKey KUp _ -> onUp
--             EvKey (KChar 'q') _ -> return Nothing
--             EvKey KEsc _ -> return Nothing
--             _ -> waitForSelection ichan currentIdx
--       where
--         onDown = waitForSelection ichan ((currentIdx + 1) `rem` numChoices)
--         onUp = let currentIdx' = if currentIdx == 0
--                                    then length choices - 1
--                                    else currentIdx - 1
--                  in waitForSelection ichan currentIdx'


listPrompt :: String -> [String] -> IO String
listPrompt question options = setup $ do
    putStrLn question
    -- selection has structure: (selected item's index, indexed options)
    let selection = (0, zip options ([0..] :: [Int]))
    mi <- listenForSelection selection
    case mi of
        Just i -> return (options !! i)
        Nothing -> exitSuccess
  where
    setup = hWithNoBuffering stdin . withNoEcho

    listenForSelection selection = do
        inp <- Vty.inputForConfig =<< Vty.standardIOConfig
        go (Vty._eventChannel inp) selection
      where
        go c os = do
            render os
            hFlush stdout
            e <- atomically (readTChan c)
            case e of
                EvKey KEnter _ -> do
                    -- makeChoice
                    return (Just (fst os))
                EvKey (KChar 'n') [MCtrl] -> do
                    clearFromCursorTo $ length $ snd os
                    go c (updateSelection MoveDown os)
                EvKey (KChar 'j') _ -> do
                    clearFromCursorTo $ length $ snd os
                    go c (updateSelection MoveDown os)
                EvKey KDown _ -> do
                    clearFromCursorTo $ length $ snd os
                    go c (updateSelection MoveDown os)
                EvKey (KChar 'p') [MCtrl] -> do
                    clearFromCursorTo $ length $ snd os
                    go c (updateSelection MoveUp os)
                EvKey (KChar 'k') _ -> do
                    clearFromCursorTo $ length $ snd os
                    go c (updateSelection MoveUp os)
                EvKey KUp _ -> do
                    clearFromCursorTo $ length $ snd os
                    go c (updateSelection MoveUp os)
                EvKey (KChar 'q') _ ->
                    return Nothing
                EvKey (KChar 'c') [MCtrl] ->
                    return Nothing
                EvKey KEsc _ ->
                    return Nothing
                _ -> go c os

        makeChoice = forM_ (replicate (length (snd selection)) ())
            (const (clearLine >> cursorUpLine 1))

    updateSelection MoveUp   (i, os) = ((i - 1) `mod` length os, os)
    updateSelection MoveDown (i, os) = ((i + 1) `mod` length os, os)
    updateSelection _ _ = error "Internal error, key not recognized"

    render (s, optionsI) = forM_ optionsI $ \(o, i) ->
        if i == s
            then do
                setSGR [ SetColor Foreground Vivid White
                       , SetConsoleIntensity BoldIntensity
                       ]
                putStr "> "
                setSGR [ SetColor Foreground Vivid Cyan
                       , SetConsoleIntensity NormalIntensity
                       ]
                putStrLn $ o
                setSGR []
            else putStrLn $ "  " ++ o

checkboxPrompt :: String -> [String] -> IO [String]
checkboxPrompt question options = setup $ do
    putStrLn question
    let selection = (0, [], zip options ([0..] :: [Int]))
    render selection
    is <- listenForSelection selection
    return $ map (options !!) is
  where
    setup = hWithNoBuffering stdin . withNoEcho

    listenForSelection :: (Int, [Int], [(String, Int)]) -> IO [Int]
    listenForSelection selection@(_, _, s3) = do
        inp <- Vty.inputForConfig =<< Vty.standardIOConfig
        go (Vty._eventChannel inp) selection
      where
        go :: TChan Event -> (Int, [Int], [(String, Int)]) -> IO [Int]
        go c os@(_, os2, os3) = do
            render os
            hFlush stdout
            e <- atomically (readTChan c)
            print e
            case e of
                EvKey KEnter _ -> do
                    makeChoice
                    return os2
                EvKey (KChar 'n') [MCtrl] -> do
                    clearFromCursorTo $ length os3
                    go c (updateSelection MoveDown os)
                EvKey (KChar 'j') _ -> do
                    clearFromCursorTo $ length os3
                    go c (updateSelection MoveDown os)
                EvKey KDown _ -> do
                    clearFromCursorTo $ length os3
                    go c (updateSelection MoveDown os)
                EvKey (KChar 'p') [MCtrl] -> do
                    clearFromCursorTo $ length os3
                    go c (updateSelection MoveUp os)
                EvKey (KChar 'k') _ -> do
                    clearFromCursorTo $ length os3
                    go c (updateSelection MoveUp os)
                EvKey KUp _ -> do
                    clearFromCursorTo $ length os3
                    go c (updateSelection MoveUp os)
                EvKey (KChar 'q') _ ->
                    return []
                EvKey (KChar 'c') [MCtrl] ->
                    return []
                EvKey KEsc _ ->
                    return []
                _ -> do
                    clearFromCursorTo $ length os3
                    go c os

        makeChoice = do
            let size = length (s3 :: [(String, Int)])
                mlist = replicate size ()
            forM_ mlist (const (clearLine >> cursorUpLine 1))

    updateSelection MoveUp   (i, is, os) = ((i - 1) `mod` length os, is, os)
    updateSelection MoveDown (i, is, os) = ((i + 1) `mod` length os, is, os)
    updateSelection ToggleSelection (i, is, os) = (i, is', os)
      where
        is' = if i `elem` is then delete i is else i:is
    updateSelection _ _ = error "Internal error, key not recognized"

    render (i, is, optionsI) = forM_ optionsI $ \(o, j) -> do
        let checkbox = if j `elem` is then "◉ " else "◯ "
        if i == j
            then do
                setSGR [ SetColor Foreground Vivid Cyan ]
                putStrLn $ ">" ++ checkbox ++ o
                setSGR []
            else putStrLn $ " " ++ checkbox ++ o
