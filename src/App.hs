{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App where

import           Relude hiding (state)
import           System.FilePath.Posix ((</>))

import qualified Data.List.PointedList as PointedList
import qualified System.Directory as Directory
import qualified System.Posix.Files as Files

import qualified Core
import qualified CLI

{-
  This module wires together the business logic (`Core` module) and the CLI
  interface (`CLI` module) in order to get a working application.
-}

-- | Base layer of our Haskell cake.
-- At first sight, this might seem pretty pointless: why wrap `IO a` instead
-- of using it directly? The answer is just below. We need a newtype so that
-- we can provide an instance for the `FileSystem` class that our application
-- needs to run.
-- Also, no scary monad transformers. Hooray!
-- Can we really write real world apps without a giant transformers stack?
-- I'm a big fan of the "ReaderT design pattern" but in this case, we don't
-- even need to go that far.
newtype App a
  = App { runApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Other half of our tasty cake middle layer.
-- This is where we define actual instances for all the abstract classes we
-- defined in our business logic. The nitty gritty, dirty, possibly IO heavy
-- code should be confined to this instances, the business logic should
-- always declare what it needs through mtl classes and have no IO in sight.
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


-- | Run the application with some state, wait for user input and repeat.
-- In a nutshell, we're just calling `Core.update` over and over until at
-- some point we get to terminate. Note how the new state is determined
-- by the business logic in the `update` function. We then feed back
-- the new state in the `loop` function again. As simple as it is, in
-- Haskell the solution to most problems is to just pass values to functions!
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
