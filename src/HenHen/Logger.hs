module HenHen.Logger
( logger
) where

import Control.Monad.IO.Class (MonadIO(..))
import System.IO
    ( Handle
    , stdout
    , stderr
    , hPutStr
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
    deriving (Show)

printColor :: (MonadIO m) => Handle -> [SGR] -> String -> m ()
printColor handle styles message = liftIO $ do
    supported <- hSupportsANSIColor handle
    if supported
        then do
            hSetSGR handle styles
            hPutStr handle message
            hSetSGR handle [Reset]
        else do
            hPutStr handle message

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

logger :: (MonadIO m) => LogType -> String -> m ()
logger logType = printColor handle styles
    where styles = getColor  logType
          handle = getHandle logType
