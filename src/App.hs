{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App where

import           Relude
import           System.FilePath.Posix ((</>))

import qualified Data.List.PointedList as PointedList
import qualified System.Directory as Directory
import qualified System.FilePath.Posix as FilePath
import qualified System.Posix.Files as Files

import qualified Core


newtype App a
  = App { runApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Core.FileSystem App where
  scanDirectory dir = do
    dirs <- liftIO $ Directory.getDirectoryContents dir

    files <- forM dirs $ \path -> do
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
