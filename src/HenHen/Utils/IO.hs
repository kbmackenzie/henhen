module HenHen.Utils.IO
( readFileSafe
, getFileModTime
, writeFileSafe
) where

import HenHen.Packager (Packager, liftIO, liftEither)
import Data.ByteString (ByteString)
import Control.Exception (catch, IOException)
import Control.Monad ((>=>))
import qualified Data.ByteString as ByteString
import System.Directory (getModificationTime)
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
