module HenHen.Utils.IO
( readFileSafe
, getFileModTime
, writeFileSafe
, fileLink
, EntryType(..)
, exists
, createDirectory
) where

import HenHen.Packager (Packager, liftIO, liftEither)
import Data.ByteString (ByteString)
import Control.Exception (catch, IOException)
import Control.Monad ((>=>))
import qualified Data.ByteString as ByteString
import System.Directory
    ( getModificationTime
    , createFileLink
    , doesFileExist
    , doesDirectoryExist
    , doesPathExist
    , createDirectoryIfMissing
    )
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

fileError :: String -> FilePath -> IOException -> String
fileError message path err = concat [ message, " ", show path, ": ", show err ] 

readFileSafe :: FilePath -> Packager ByteString
readFileSafe path = (liftIO >=> liftEither) $ do
    let reader = fmap Right . ByteString.readFile
    reader path `catch` \err -> do
        let message = fileError "couldn't read file" path err
        return $ Left message

getFileModTime :: FilePath -> Packager Integer
getFileModTime path = (liftIO >=> liftEither) $ do
    let getter = fmap Right . getModificationTime
    utcTime <- getter path `catch` \err -> do
        let message = fileError "couldn't get modification date for file" path err
        return $ Left message
    return $ fmap (round . utcTimeToPOSIXSeconds) utcTime


writeFileSafe :: FilePath -> ByteString -> Packager ()
writeFileSafe path content = (liftIO >=> liftEither) $ do
    let writer = (fmap Right .) . ByteString.writeFile
    writer path content `catch` \err -> do
        let message = fileError "couldn't write file" path err
        return $ Left message

fileLink :: FilePath -> FilePath -> Packager ()
fileLink input output = (liftIO >=> liftEither) $ do
    let link = (fmap Right .) . createFileLink
    link input output `catch` \err -> do
        let message = fileError "couldn't create symlink" output err
        return $ Left message

data EntryType = Any | File | Directory
    deriving (Eq, Ord, Enum, Bounded, Show)

exists :: EntryType -> FilePath -> Packager Bool
exists entry path = (liftIO >=> liftEither) $ do
    let check = fmap Right . case entry of
            Any       -> doesPathExist
            File      -> doesFileExist
            Directory -> doesDirectoryExist
    check path `catch` \err -> do
        let message = fileError "couldnt't check existence" path err
        return $ Left message

createDirectory :: Bool -> FilePath -> Packager ()
createDirectory recursive path = (liftIO >=> liftEither) $ do
    let create = fmap Right . createDirectoryIfMissing recursive
    create path `catch` \err -> do
        let message = fileError "couldn't create directory" path err
        return $ Left message
