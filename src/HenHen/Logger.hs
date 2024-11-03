module HenHen.Logger
( Logger(..)
, LogType(..)
, LogLevel(..)
, logMessage
, logVerbose
, defaultLogger
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

data Logger = Logger
    { logType  :: LogType
    , logLevel :: LogLevel }
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

getColor :: Logger -> [SGR]
getColor logInfo = case logType logInfo of
    Info    -> []
    Error   -> [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
    Warning -> [SetColor Foreground Vivid Yellow]

getHandle :: Logger -> Handle
getHandle logInfo = case logType logInfo of
    Info    -> stdout
    Error   -> stderr
    Warning -> stdout

logMessage :: (MonadIO m) => Logger -> String -> m ()
logMessage logInfo = unless quiet . printColor handle styles
    where styles = getColor  logInfo
          handle = getHandle logInfo
          quiet  = logLevel logInfo == Quiet

logVerbose :: (MonadIO m) => Logger -> String -> m ()
logVerbose logInfo = when verbose . printColor handle styles
    where styles  = getColor  logInfo
          handle  = getHandle logInfo
          verbose = logLevel logInfo == Verbose

defaultLogger :: Logger
defaultLogger = Logger Info Normal
