{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App where

import           Relude hiding (state)
import           System.FilePath.Posix ((</>))

import qualified Data.List.PointedList as PointedList
import qualified System.Directory as Directory
import qualified System.Posix.Files as Files

import qualified Core
import qualified CLI


newtype App a
  = App { runApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Core.FileSystem App where
  scanDirectory dir = do
    dirContent <- liftIO $ Directory.listDirectory dir

    files <- forM (dirContent <> [".."]) $ \path -> do
      fileType <- liftIO $ getFileType (dir </> path)
      pure Core.File
            { Core.filePath = path
            , Core.fileType = fileType
            }

    case PointedList.fromList files of
      Just result -> pure result
      Nothing     -> error "no files found" -- TODO return Left someerror

  resolvePath
    = liftIO . Directory.canonicalizePath

  homeDirectoryPath
    = liftIO Directory.getHomeDirectory

getFileType :: FilePath -> IO Core.FileType
getFileType path = do
  status <- Files.getSymbolicLinkStatus path
  go status

  where
    go status
      | Files.isDirectory status    = pure Core.Folder
      | Files.isSymbolicLink status = doSymbolic
      | otherwise                   = pure Core.NormalFile

    doSymbolic = do
      link <- Files.readSymbolicLink path
      pure (Core.SymbolicLink link)

main :: IO ()
main = do
  result <-
    CLI.run $ runApp $ do
      state <- Core.newStateFromFolder "."
      loop state

  case result of
    Just finalPath -> putStrLn finalPath
    Nothing        -> pure ()

loop :: Core.State -> App (Maybe FilePath)
loop state = do
  event <- liftIO $ CLI.render state

  case event of
    CLI.Quit ->
      pure Nothing

    CLI.UnrecognizedInput _ ->
      loop state

    CLI.AppCmd cmd -> do
      result <- Core.update state cmd
      case result of
        Core.Running newState ->
          loop newState
        Core.FileSelected path ->
          pure (Just path)
