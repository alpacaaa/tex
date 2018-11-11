module Core where

import           Relude hiding (State)
import           System.FilePath.Posix ((</>))

import qualified Data.List as List
import qualified Data.List.PointedList as PointedList
import qualified Data.Map.Strict as Map


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
  | SymbolicLink FilePath
  deriving (Show, Eq)

data File
  = File
      { filePath :: FilePath
      , fileType :: FileType
      }
  deriving (Show, Eq)

data Cmd
  = JumpNext
  | JumpPrev
  | JumpParentFolder
  | SelectCurrentFile
  deriving (Show)

data UpdateResult
  = FileSelected FilePath
  | Running State

class Monad m => FileSystem m where
  scanDirectory :: FilePath -> m FilesList
  resolvePath   :: FilePath -> m FilePath

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

  JumpParentFolder -> do
    let parent = currentFullPath </> ".."
    newFiles <- scanAndSortFolder parent
    running $ state
                { files = newFiles
                , currentPath = parent
                }

  SelectCurrentFile -> do
    case fileType current of
      Folder -> do
        newFiles <- scanAndSortFolder currentFullPath
        running $ state
                    { files = newFiles
                    , currentPath = currentFullPath
                    }

      NormalFile -> do
        canonical <- resolvePath currentFullPath
        pure $ FileSelected canonical

      SymbolicLink _ -> do
        canonical <- resolvePath currentFullPath
        pure $ FileSelected canonical

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
      = sortFn folders <> sortFn rest

    sortFn
      = sortOn filePath

    (folders, rest)
      = List.partition (\f -> fileType f == Folder) (toList files)

currentIndex :: State -> Int
currentIndex state
  = PointedList.index (files state)

fileDisplayName :: File -> String
fileDisplayName file
  = filePath file
    <> case fileType file of
        NormalFile        -> ""
        Folder            -> "/"
        SymbolicLink link -> " -> " <> link
