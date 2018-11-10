module Core where

import           Relude hiding (State)
import           System.FilePath.Posix ((</>))

import qualified Data.List.PointedList as PointedList
import qualified Data.Map.Strict as Map
import qualified Data.Ord


type FilesList
  = PointedList.PointedList File

data State
  = State
      { files        :: FilesList
      , currentPath  :: FilePath
      , originalPath :: FilePath
      }
  deriving (Show)

data FileType
  = NormalFile
  | Folder
  deriving (Show, Eq, Ord)

data File
  = File
      { filePath :: FilePath
      , fileType :: FileType
      }
  deriving (Show, Eq, Ord)

data Cmd
  = JumpNext
  | JumpPrev
  | SelectCurrentFile
  deriving (Show)

data UpdateResult
  = FileSelected FilePath
  | Running State

class Monad m => FileSystem m where
  scanDirectory :: FilePath -> m FilesList

update :: (FileSystem m) => State -> Cmd -> m UpdateResult
update state = \case
  JumpNext ->
    case PointedList.next (files state) of
      Just newFiles ->
        running $ state { files = newFiles }
      Nothing ->
        running state

  JumpPrev ->
    case PointedList.previous (files state) of
      Just newFiles ->
        running $ state { files = newFiles }
      Nothing ->
        running state

  SelectCurrentFile -> do
    case fileType current of
      Folder -> do
        newFiles <- scanAndSortFolder currentFullPath
        running $ state
                    { files = newFiles
                    , currentPath = currentFullPath
                    }

      NormalFile ->
        pure $ FileSelected currentFullPath

  where
    running newState
      = pure (Running newState)

    current
      = PointedList._focus (files state)

    currentFullPath
      = currentPath state </> filePath current

scanAndSortFolder :: FileSystem m => FilePath -> m FilesList
scanAndSortFolder path = do
  files <- scanDirectory path
  pure (sortFiles files)

newStateFromFolder :: FileSystem m => FilePath -> m State
newStateFromFolder path = do
  files <- scanAndSortFolder path
  pure State
        { files        = files
        , currentPath  = path
        , originalPath = path
        }

-- TODO sorting is funky. Needs to take sorting mode.
sortFiles :: FilesList -> FilesList
sortFiles files
  = case PointedList.fromList sorted of
      Just result ->
        result
      Nothing ->
        -- We know the list has at least one element
        error "impossible sortFiles"
  where
    sorted
      = sortOn
          (Data.Ord.Down . fileType)
          (toList files)

currentIndex :: State -> Int
currentIndex state
  = PointedList.index (files state)
