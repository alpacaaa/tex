module Core where

import           Relude hiding (State)
import           System.FilePath.Posix ((</>))

import qualified Data.List.PointedList as PointedList
import qualified Data.Map.Strict as Map
import qualified Data.Ord


data Env
  = Env
      { windowSize   :: (Int, Int)
      , originalPath :: FilePath
      }
  deriving (Show)

type FilesList
  = PointedList.PointedList File

data State
  = State
      { files       :: FilesList
      , currentPath :: FilePath
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

class Monad m => FileSystem m where
  scanDirectory :: FilePath -> m FilesList

update :: (FileSystem m) => State -> Cmd -> m State
update state = \case
  JumpNext ->
    case PointedList.next (files state) of
      Just newFiles ->
        pure $ state { files = newFiles }
      Nothing ->
        pure state

  JumpPrev ->
    case PointedList.previous (files state) of
      Just newFiles ->
        pure $ state { files = newFiles }
      Nothing ->
        pure state

  SelectCurrentFile -> do
    let current = PointedList._focus (files state)
    case fileType current of
      Folder ->
        newStateFromFolder
          $ currentPath state </> filePath current
      NormalFile ->
        pure state

newStateFromFolder :: FileSystem m => FilePath -> m State
newStateFromFolder path = do
  newFiles <- scanDirectory path
  pure State
        { files       = sortFiles newFiles
        , currentPath = path
        }

sortFiles :: FilesList -> FilesList
sortFiles files
  = case PointedList.fromList sortedFiles of
      Just result ->
        result
      Nothing ->
        -- We know the list has at least one element
        error "impossible sortFiles"
  where
    sortedFiles
      = sortOn
          (Data.Ord.Down . fileType)
          (toList files)
