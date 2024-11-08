{-# LANGUAGE OverloadedStrings #-}

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
import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , withText
    , Value(String)
    )
import qualified Data.Text as Text

data LogType = Info | Warning | Error
    deriving (Eq, Show, Ord, Enum, Bounded)

data LogLevel = Quiet | Normal | Verbose
    deriving (Eq, Show, Ord, Enum, Bounded)

instance FromJSON LogLevel where
    parseJSON = withText "LogLevel" $ \text -> case normalize text of
        "normal"  -> return Normal
        "quiet"   -> return Quiet
        "verbose" -> return Verbose
        other     -> fail ("Unrecognized log level value: " ++ show other)
        where normalize = Text.toLower . Text.strip

instance ToJSON LogLevel where
    toJSON logLevel = String $ case logLevel of
        Normal  -> "normal"
        Quiet   -> "quiet"
        Verbose -> "verbose"

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

logMessage :: (MonadIO m) => Maybe LogLevel -> String -> m ()
logMessage logLevel = unless quiet . printColor handle styles
    where styles = getColor  Info
          handle = getHandle Info
          quiet  = logLevel == Just Quiet

logVerbose :: (MonadIO m) => Maybe LogLevel -> String -> m ()
logVerbose logLevel = when verbose . printColor handle styles
    where styles  = getColor  Info
          handle  = getHandle Info
          verbose = logLevel == Just Verbose

logWarning :: (MonadIO m) => Maybe LogLevel -> String -> m ()
logWarning logLevel = unless quiet . printColor handle styles
    where styles = getColor  Warning
          handle = getHandle Warning
          quiet  = logLevel == Just Quiet

logError :: (MonadIO m) => String -> m ()
logError = printColor handle styles
    where styles = getColor  Error
          handle = getHandle Error
