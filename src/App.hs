{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App where

import           Relude
import           System.FilePath.Posix ((</>))

import qualified Data.List.PointedList as PointedList
import qualified System.Directory as Directory
import qualified System.FilePath.Posix as FilePath
import qualified System.Posix.Files as Files

import qualified Core
import qualified Buffer


newtype App a
  = App { runApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Core.FileSystem App where
  scanDirectory dir = do
    dirContent <- liftIO $ Directory.getDirectoryContents dir

    files <- forM dirContent $ \path -> do
      status <- liftIO $ Files.getFileStatus (dir </> path)
      pure Core.File
            { Core.filePath
                = path
            , Core.fileType
                = if Files.isRegularFile status
                    then Core.NormalFile
                    else Core.Folder
            }

    case PointedList.fromList files of
      Just result -> pure result
      Nothing     -> error "no files found" -- TODO return Left someerror

main :: IO ()
main = do
  Buffer.run $ runApp $ do
    let path = "."
    state <- Core.newStateFromFolder path
    loop state

loop :: Core.State -> App ()
loop state = do
  event <- liftIO $ Buffer.render state
  case event of
    Buffer.Quit ->
      pure ()
    Buffer.UnrecognizedInput _ ->
      loop state
    Buffer.AppCmd cmd -> do
      newState <- Core.update state cmd
      loop newState
