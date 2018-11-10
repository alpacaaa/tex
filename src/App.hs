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
  result <-
    Buffer.run $ runApp $ do
      state <- Core.newStateFromFolder "."
      loop state

  case result of
    Just finalPath -> putStrLn finalPath
    Nothing        -> pure ()

loop :: Core.State -> App (Maybe FilePath)
loop state = do
  event <- liftIO $ Buffer.render state

  case event of
    Buffer.Quit ->
      pure Nothing

    Buffer.UnrecognizedInput _ ->
      loop state

    Buffer.AppCmd cmd -> do
      result <- Core.update state cmd
      case result of
        Core.Running newState ->
          loop newState
        Core.FileSelected path ->
          pure (Just path)
