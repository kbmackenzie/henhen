module HenHen.Utils.IO
( readFileSafe
, getFileModTime
, writeFileSafe
, EntryType(..)
, exists
, createDirectory
, globFiles
, copyFileSafe
, removeDirectory
, removeDirectoryIfExists
, removeFileSafe
, createFileLinkSafe
) where

import HenHen.Packager (Packager, liftIO, liftEither)
import Data.ByteString (ByteString)
import Control.Exception (catch, IOException)
import Control.Monad ((>=>), when)
import qualified Data.ByteString as ByteString
import System.FilePath ((</>))
import System.Directory
    ( getModificationTime
    , createFileLink
    , doesFileExist
    , doesDirectoryExist
    , doesPathExist
    , createDirectoryIfMissing
    , copyFile
    , removeDirectoryRecursive
    , removeFile
    , removePathForcibly
    , canonicalizePath
    )
import System.FilePattern.Directory
    ( FilePattern
    , getDirectoryFiles
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

globFiles :: FilePath -> [FilePattern] -> Packager [FilePath]
globFiles path patterns = (liftIO >=> liftEither) $ do
    let root = (path </>)
    let glob = (fmap (Right . map root) .) . getDirectoryFiles
    glob path patterns `catch` \err -> do
        let message = fileError "couldn't glob files in path" path err
        return $ Left message

copyFileSafe :: FilePath -> FilePath -> Packager ()
copyFileSafe input output = (liftIO >=> liftEither) $ do
    let copy = (fmap Right .) . copyFile
    copy input output `catch` \err -> do
        let message = fileError "couldn't copy file" input err
        return $ Left message

removeDirectory :: FilePath -> Packager ()
removeDirectory path = (liftIO >=> liftEither) $ do
    let rm = fmap Right . removeDirectoryRecursive
    rm path `catch` \err -> do
        let message = fileError "couldn't remove directory" path err
        return $ Left message

removeDirectoryIfExists :: FilePath -> Packager ()
removeDirectoryIfExists path = do
    hasDirectory <- exists Directory path
    when hasDirectory (removeDirectory path)

removeFileSafe :: FilePath -> Packager ()
removeFileSafe path = (liftIO >=> liftEither) $ do
    let rm = fmap Right . removeFile
    print "removing..."
    rm path `catch` \err -> do
        let message = fileError "couldn't remove file" path err
        return $ Left message

createFileLinkSafe :: Bool -> FilePath -> FilePath -> Packager ()
createFileLinkSafe overwrite target symlink = do
    let link :: IO ()
        link = do
            when overwrite (removePathForcibly symlink)
            targetPath <- canonicalizePath target
            createFileLink targetPath symlink
    (liftIO >=> liftEither) $ do
        fmap Right link `catch` \err -> do
            let message = fileError "couldn't create symlink" symlink err
            return $ Left message
