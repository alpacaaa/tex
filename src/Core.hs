module Core where

import           Relude hiding (State, state)
import           System.FilePath.Posix ((</>))

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.PointedList as PointedList


type FilesList
  = PointedList.PointedList File

data State
  = State
      { files         :: FilesList
      , currentPath   :: FilePath
      , originalPath  :: FilePath
      , homeDirectory :: FilePath
      , currentMode   :: Mode
      , searchPattern :: SearchPattern
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

newtype SearchPattern
  = SearchPattern String
  deriving (Show, Eq)

data Mode
  = ModeNavigation
  | ModeSearch
  deriving (Show, Eq)

data Cmd
  = JumpNext
  | JumpPrev
  | JumpMany Int
  | JumpParentFolder
  | JumpHomeDirectory
  | JumpBeginning
  | JumpEnd
  | SelectCurrentFile
  | SwitchMode Mode
  | UpdateSearch SearchPattern
  | SearchNextMatch
  | CommitSearch
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
    tryNewFiles $ PointedList.next (files state)

  JumpPrev ->
    tryNewFiles $ PointedList.previous (files state)

  JumpMany n ->
    tryNewFiles $ PointedList.moveN jump filesList
    where
      filesList = files state
      jump = findSensibleJump filesList n

  JumpParentFolder -> do
    newState <- switchFolder state (currentPath state </> "..")
    running newState

  JumpHomeDirectory -> do
    newState <- switchFolder state (homeDirectory state)
    running newState

  JumpBeginning -> do
    tryNewFiles $ PointedList.moveTo 0 (files state)

  JumpEnd -> do
    tryNewFiles $ PointedList.moveTo end filesList
    where
      filesList = files state
      end = (PointedList.length filesList) - 1

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

  SwitchMode mode ->
    running $ state { currentMode = mode }

  UpdateSearch search ->
    running $ state { searchPattern = search }

  CommitSearch ->
    running
      $ selectNextSearchMatch Forward
      $ state { currentMode = ModeNavigation }

  SearchNextMatch ->
    running $ selectNextSearchMatch Forward state

  where
    running newState
      = pure (Running newState)

    current
      = PointedList._focus (files state)

    currentFullPath
      = currentPath state </> filePath current

    tryNewFiles = \case
      Just newFiles ->
        running $ state { files = newFiles }
      Nothing ->
        running state

scanAndSortFolder :: FileSystem m => FilePath -> m FilesList
scanAndSortFolder path = do
  filesList <- scanDirectory path
  pure (sortFiles filesList)

newStateFromFolder :: FileSystem m => FilePath -> m State
newStateFromFolder path = do
  canonical <- resolvePath path
  filesList <- scanAndSortFolder canonical
  home <- homeDirectoryPath

  pure State
        { files         = filesList
        , currentPath   = canonical
        , originalPath  = canonical
        , homeDirectory = home
        , currentMode   = ModeNavigation
        , searchPattern = SearchPattern ""
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
sortFiles filesList
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
      = List.partition
          (\f -> fileType f == Folder)
          (toList filesList)

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

-- TODO this is clearly very naive, we probably
-- want regular expressions here
matchPattern :: SearchPattern -> FilePath -> Bool
matchPattern (SearchPattern search) path
  = List.isInfixOf (toLower search) (toLower path)
  where
    toLower = map Char.toLower

data Movement
  = Forward
  | Backward
  deriving (Show, Eq)

-- | Given a PointedList and a predicate, returns a
-- | modified PointedList where the focus is on the
-- | first element that matches the predicate. Loop
-- | starts at current focus.
moveFocus
  :: (a -> Bool)
  -> Movement
  -> PointedList.PointedList a
  -> PointedList.PointedList a
moveFocus predicate movement list
  = fromMaybe list
  $ case foundIndex of
      Just index ->
        if index >= suffixLen
          then let move = index - suffixLen
               in PointedList.moveTo move list
          else
            PointedList.moveN (index + 1) list
      Nothing ->
        Nothing

  where
    foundIndex
      = List.findIndex predicate targetList

    suffix
      = PointedList._suffix list

    suffixLen
      = List.length suffix

    prefix
      = PointedList._reversedPrefix list

    targetList
      = case movement of
          Forward  -> suffix <> List.reverse prefix
          Backward -> prefix <> suffix

selectNextSearchMatch :: Movement -> State -> State
selectNextSearchMatch movement state
  = state { files = updated }
  where
    updated
      = searchNext movement (searchPattern state) (files state)

searchNext
  :: Movement
  -> SearchPattern
  -> FilesList
  -> FilesList
searchNext movement search filesList
  = moveFocus match movement filesList
  where
    match file
      = matchPattern search (filePath file)
