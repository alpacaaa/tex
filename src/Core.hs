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
      { files         :: FilesList
      , currentPath   :: FilePath
      , originalPath  :: FilePath
      , homeDirectory :: FilePath
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
  | JumpMany Int
  | JumpParentFolder
  | JumpHomeDirectory
  | SelectCurrentFile
  deriving (Show)

data UpdateResult
  = FileSelected FilePath
  | Running State

class Monad m => FileSystem m where
  scanDirectory :: FilePath -> m FilesList
  resolvePath   :: FilePath -> m FilePath
  homeDirectoryPath :: m FilePath

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

  JumpMany n ->
    case PointedList.moveN jump filesList of
      Just newFiles ->
        running $ state { files = newFiles }
      Nothing ->
        running state
    where
      filesList = files state
      jump = findSensibleJump filesList n

  JumpParentFolder -> do
    newState <- switchFolder state (currentPath state </> "..")
    running newState

  JumpHomeDirectory -> do
    newState <- switchFolder state (homeDirectory state)
    running newState

  SelectCurrentFile -> do
    case fileType current of
      Folder -> do
        newState <- switchFolder state currentFullPath
        running newState

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
  canonical <- resolvePath path
  files <- scanAndSortFolder canonical
  home <- homeDirectoryPath

  pure State
        { files         = files
        , currentPath   = canonical
        , originalPath  = canonical
        , homeDirectory = home
        }

switchFolder :: FileSystem m => State -> FilePath -> m State
switchFolder state path = do
  canonical <- resolvePath path
  newFiles  <- scanAndSortFolder canonical
  pure state
        { files       = newFiles
        , currentPath = canonical
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

-- When using ctrl-d or ctrl-f the jump can be
-- greater than how many items are actually in
-- there. So it must be capped at at most the
-- number of items, depending on the direction
findSensibleJump :: FilesList -> Int -> Int
findSensibleJump filesList n
  = if n > 0
      then min n nextLen
      else max n $ prevLen * (-1)
  where
    nextLen = length (PointedList._suffix filesList)
    prevLen = length (PointedList._reversedPrefix filesList)
