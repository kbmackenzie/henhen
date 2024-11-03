module HenHen.Logger
( LogLevel(..)
, logMessage
, logVerbose
, logWarning
, logError
) where

import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO(..))
import System.IO
    ( Handle
    , stdout
    , stderr
    , hPutStrLn
    )
import System.Console.ANSI
    ( SGR(..)
    , Color(..)
    , ColorIntensity(..)
    , ConsoleLayer(..)
    , ConsoleIntensity(..)
    , hSetSGR
    , hSupportsANSIColor
    )

data LogType = Info | Error | Warning
    deriving (Eq, Show)

data LogLevel = Quiet | Normal | Verbose
    deriving (Eq, Show)

printColor :: (MonadIO m) => Handle -> [SGR] -> String -> m ()
printColor handle styles message = liftIO $ do
    supported <- hSupportsANSIColor handle
    if supported
        then do
            hSetSGR handle styles
            hPutStrLn handle message
            hSetSGR handle [Reset]
        else do
            hPutStrLn handle message

getColor :: LogType -> [SGR]
getColor logType = case logType of
    Info    -> []
    Error   -> [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
    Warning -> [SetColor Foreground Vivid Yellow]

getHandle :: LogType -> Handle
getHandle logType = case logType of
    Info    -> stdout
    Error   -> stderr
    Warning -> stdout

logMessage :: (MonadIO m) => LogLevel -> String -> m ()
logMessage logLevel = unless quiet . printColor handle styles
    where styles = getColor  Info
          handle = getHandle Info
          quiet  = logLevel == Quiet

logVerbose :: (MonadIO m) => LogLevel -> String -> m ()
logVerbose logLevel = when verbose . printColor handle styles
    where styles  = getColor  Info
          handle  = getHandle Info
          verbose = logLevel == Verbose

logWarning :: (MonadIO m) => LogLevel -> String -> m ()
logWarning logLevel = unless quiet . printColor handle styles
    where styles = getColor  Warning
          handle = getHandle Warning
          quiet  = logLevel == Quiet

logError :: (MonadIO m) => String -> m ()
logError = printColor handle styles
    where styles = getColor  Error
          handle = getHandle Error
