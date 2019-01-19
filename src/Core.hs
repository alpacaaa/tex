module Core where

import           Relude hiding (State, state)
import           System.FilePath.Posix ((</>))

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.PointedList as PointedList

{-
  This module contains all the business logic, defined in the most
  abstract possible way. We declare a `FileSystem` monad to describe the
  operations we need to perform, but defer the actual implementation for
  later (a concrete implementation can be found in `App` while tests
  use a mock instance).

  In Haskell cake parlance, this would be the third layer (just pure functions
  and simple types).

  Important data types in here are `State` and `Cmd`.
  The most important function is probably `update`.
-}

-- | The list of files in the current directory.
type FilesList
  = PointedList.PointedList File

-- | Application state.
-- Everything we could possibly ever need should be carried around here.
-- There's no global state or mutable state anywhere, at the end of the day
-- it's just a matter of carring around a `State` value, reading from it to
-- render the screen and updating it when needed. Almost sounds too boring :)
data State
  = State
      { files           :: FilesList
      , currentPath     :: FilePath
      , originalPath    :: FilePath
      , homeDirectory   :: FilePath
      , currentMode     :: Mode
      , searchPattern   :: SearchPattern
      , searchDirection :: Direction
      , history         :: History
      }
  deriving (Show)

-- | Which files are we working with?
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

data Direction
  = Forward
  | Backward
  deriving (Show, Eq)

-- | Entering or exiting a directory shouldn't be a "destructive" operation.
-- We just store whichever directories we might need to undo/redo an operation.
data History
  = History
      { undoPaths :: [FilePath]
      , redoPaths :: [FilePath]
      }
  deriving (Show, Eq)

-- | At any given time, we're either navigating the directory tree, or we're
-- typing in the search box.
data Mode
  = ModeNavigation
  | ModeSearch
  deriving (Show, Eq)

-- | These are all the operations we support. In other words, our application
-- can handle any of these commands, it's the UI responsability to determine
-- which command should be fed into the app loop.
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
  | SwitchSearchMode Direction
  | UpdateSearch SearchPattern
  | SearchNextMatch
  | SearchPrevMatch
  | CommitSearch
  | Undo
  | Redo
  deriving (Show)

-- | Whenever we run the `update` function, we could be in one of two states.
-- If the user has selected a file, we're done and the app should stop.
data UpdateResult
  = FileSelected FilePath
  | Running State

-- | This belongs to the second layer (yes, I'm talking about cakes
-- again; stay focused!) but I don't think it warranted a separate module.
-- Thanks to mtl, we can just declare what our business logic needs in order to
-- work, *without* constraining ourselves to any monad or ever talking about IO.
class Monad m => FileSystem m where
  scanDirectory :: FilePath -> m FilesList
  resolvePath   :: FilePath -> m FilePath
  homeDirectoryPath :: m FilePath

-- | Main function that determines what happens next.
-- Note the constraint: provided we're running in a monad that supports
-- `FileSystem` operations, then given the current state and a command,
-- we can determine if the app should still be running (with an updated state)
-- or it should exit.

{-
  Pay close attention to how easy it is to work with plain and simple data
  types. In the few cases where we actually *need* to do something (ie.
  scan all the files in a directory), we can still write nice and abstract
  code that uses methods from the `FileSystem` class, even though we don't
  know what the actual implementation is going to be when the function is run.

  This function could be split further, as it's fairly big already. The beauty
  of working with functions is that you can just compose them, so refactoring
  is definitely not going to be a problem here ;)
-}
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
    home <- homeDirectoryPath
    newState <- switchFolder state home
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

  SwitchSearchMode direction ->
    running $ state
      { currentMode     = ModeSearch
      , searchDirection = direction
      , searchPattern   = SearchPattern ""
      }

  UpdateSearch search ->
    running $ state { searchPattern = search }

  CommitSearch ->
    running
      $ selectNextSearchMatch (searchDirection state)
      $ state { currentMode = ModeNavigation }

  SearchNextMatch ->
    running $ selectNextSearchMatch Forward state

  SearchPrevMatch ->
    running $ selectNextSearchMatch Backward state

  Undo ->
    navigateHistory (undoPaths $ history state)

  Redo ->
    navigateHistory (redoPaths $ history state)

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

    navigateHistory = \case
      []    -> running state
      (x:_) -> do
        newState <- switchFolder state x
        running newState

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
        { files           = filesList
        , currentPath     = canonical
        , originalPath    = canonical
        , homeDirectory   = home
        , currentMode     = ModeNavigation
        , searchPattern   = SearchPattern ""
        , searchDirection = Forward
        , history         = History [] []
        }

switchFolder :: FileSystem m => State -> FilePath -> m State
switchFolder state path = do
  canonical <- resolvePath path
  newFiles  <- scanAndSortFolder canonical
  let newHistory
        = updateHistory (history state) (currentPath state) canonical

  pure state
        { files       = newFiles
        , currentPath = canonical
        , history     = newHistory
        }

updateHistory :: History -> FilePath -> FilePath -> History
updateHistory (History undo redo) current newPath
  | newPath `List.elem` redo      = History (current:undo) (drop 1 redo)
  | safeHead undo == Just newPath = History (drop 1 undo)  (current:redo)
  | otherwise                     = History (current:undo) []

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

-- | Find the index of the next search result (if possible).
searchNext :: Direction -> SearchPattern -> FilesList -> Maybe Int
searchNext direction search filesList
  = fst <$> nextMatch
  where
    nextMatch
      = case direction of
          Forward  -> safeHead (greater <> lesser)
          Backward -> safeHead (reverse lesser <> reverse greater)

    (greater, lesser)
      = List.partition (\(i, _) -> i > selectedIndex) allMatches

    allMatches
      = filter match indexedList

    indexedList
      = zipWith (,) [0..] (toList filesList)

    match (index, file)
      = if index /= selectedIndex
        then matchPattern search (filePath file)
        else False

    selectedIndex
      = PointedList.index filesList

selectNextSearchMatch :: Direction -> State -> State
selectNextSearchMatch direction state
  = case result of
      Just newFiles -> state { files = newFiles }
      Nothing -> state
  where
    filesList
      = files state

    result = do
      index <- searchNext direction (searchPattern state) filesList
      PointedList.moveTo index filesList
