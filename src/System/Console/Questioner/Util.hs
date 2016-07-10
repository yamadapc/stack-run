module System.Console.Questioner.Util
  where

import Control.Exception (bracket_)
import System.Console.ANSI (clearLine, cursorDownLine, cursorUpLine,
                            hideCursor, showCursor)
import System.IO (BufferMode(..), Handle, hGetBuffering, hSetBuffering,
                  hSetEcho, stdin)

-- |
-- Performs an IO action with some buffer mode on a handle
hWithBufferMode :: Handle -> BufferMode -> IO a -> IO a
hWithBufferMode handle bufferMode action = do
    originalBuffering <- hGetBuffering handle
    bracket_
        (hSetBuffering handle bufferMode)
        (hSetBuffering handle originalBuffering)
        action

-- |
-- Performs an IO action with NoBuffering on a handle
hWithNoBuffering :: Handle -> IO a -> IO a
hWithNoBuffering handle = hWithBufferMode handle NoBuffering

-- |
-- Performs an IO action with the console cursor hidden
withNoCursor :: IO a -> IO a
withNoCursor = bracket_ hideCursor showCursor

-- |
-- Performs an IO action with console "echoing" supressed
withNoEcho :: IO a -> IO a
withNoEcho = bracket_ (hSetEcho stdin False) (hSetEcho stdin True)

-- |
-- Clears the screen from the cursor's current position until `n` lines
-- above it
clearFromCursorTo :: Int -> IO ()
clearFromCursorTo nlines = do
    cursorUpLine nlines
    loop nlines
    cursorUpLine nlines
  where
    loop 0 = return ()
    loop n = do
        clearLine
        cursorDownLine 1
        loop (n - 1)
