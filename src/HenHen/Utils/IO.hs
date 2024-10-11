module HenHen.Utils.IO
( readFileSafe
) where

import HenHen.Packager (Packager, liftIO, liftEither)
import Data.ByteString (ByteString)
import Control.Exception (catch, IOException)
import Control.Monad ((>=>))
import qualified Data.ByteString as ByteString

readFileSafe :: FilePath -> Packager ByteString
readFileSafe path = (liftIO >=> liftEither) $ do
    let reader = fmap Right . ByteString.readFile
    reader path `catch` \e -> do
        let errMessage = concat [ "couldn't read file ", show path, ": ", show (e :: IOException) ]
        return $ Left errMessage
