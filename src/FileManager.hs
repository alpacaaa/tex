{-# LANGUAGE NamedFieldPuns #-}

module FileManager where

import Relude hiding (State)
import qualified Data.Ord
import qualified Data.List.PointedList as PointedList
import qualified Data.Map.Strict as Map
import qualified System.Directory as Directory
import qualified System.Posix.Files as Files
import qualified Termbox

import qualified Debug.Trace as Debug


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

data DirectoryState
  = DirectoryState
      { files :: PointedList.PointedList File
      }
  deriving (Show)

data State
  = State
      { directoryState :: DirectoryState
      , cursorPosition :: Point
      , windowSize     :: (Int, Int)
      }
  deriving (Show)

type Point
  = (Int, Int)

type Buffer
  = Map Point Termbox.Cell

data Cmd
  = Quit
  | JumpNext
  | JumpPrev
  | SelectFile FilePath
  | SelectFolder FilePath

run :: IO ()
run = do
  state <- initApp
  Termbox.main $ render state

initApp :: IO State
initApp = do
  directoryState <-
    toDirectoryState <$> scanCurrentDirectory

  pure State
        { directoryState = directoryState
        , cursorPosition = (0, 1)
        , windowSize     = (0, 0)
        }

scanCurrentDirectory :: IO [File]
scanCurrentDirectory = do
  dirs <- Directory.getDirectoryContents "."

  forM dirs $ \path -> do
    status <- Files.getFileStatus path
    pure File
          { filePath
              = path
          , fileType
              = if Files.isRegularFile status
                  then NormalFile
                  else Folder
          }

toDirectoryState :: [File] -> DirectoryState
toDirectoryState files
  = DirectoryState
      { files = pointedFiles
      }
  where
    sortedFiles
      = sortOn (Data.Ord.Down . fileType) files

    pointedFiles
      = case uncons sortedFiles of
          Just (focus, after) ->
            PointedList.PointedList [] focus after
          Nothing             ->
            error "impossible toDirectoryState"

render :: State -> IO ()
render state = do
  Termbox.clear mempty mempty
  Termbox.hideCursor

  windowSize <- Termbox.size

  let state' = state { windowSize }

  let buffer
        = drawCursor state' mempty
        & renderTree state'

  renderBuffer buffer

  Termbox.flush

  event <- Termbox.poll
  case eventToCmd state event of
    Just cmd -> handleCmd cmd state
    Nothing  -> render state

eventToCmd :: State -> Termbox.Event -> Maybe Cmd
eventToCmd state = \case
  Termbox.EventKey Termbox.KeyCtrlC _ ->
    Just Quit

  Termbox.EventKey Termbox.KeyEnter _ ->
    case fileType current of
      NormalFile -> Just $ SelectFile (filePath current)
      Folder     -> Just $ SelectFolder (filePath current)

  Termbox.EventKey (Termbox.KeyChar key) _  ->
    case key of
      'j' -> moveDown
      'k' -> moveUp
      _   -> Nothing

  Termbox.EventKey Termbox.KeyArrowDown _ ->
    moveDown

  Termbox.EventKey Termbox.KeyArrowUp _ ->
    moveUp

  _ ->
    Nothing

  where
    current
      = PointedList._focus fileList

    fileList
      = files (directoryState state)

    moveUp
      =   const JumpPrev
      <$> PointedList.previous fileList

    moveDown
      =   const JumpNext
      <$> PointedList.next fileList

handleCmd :: Cmd -> State -> IO ()
handleCmd cmd state
  = case cmd of
      Quit ->
        pure ()

      JumpNext ->
        let
          dirState
            = directoryState state

          newDirectoryState
            = dirState { files = PointedList.tryNext (files dirState) }

          (x, y)
            = cursorPosition state

        in render $ state
            { directoryState = newDirectoryState
            , cursorPosition = (x, y + 1)
            }

      JumpPrev ->
        let
          dirState
            = directoryState state

          newDirectoryState
            = dirState { files = PointedList.tryPrevious (files dirState) }

          (x, y)
            = cursorPosition state

        in render $ state
            { directoryState = newDirectoryState
            , cursorPosition = (x, y - 1)
            }

      SelectFile file ->
        pure ()

      SelectFolder path -> do
        Directory.setCurrentDirectory path

        directoryState <-
          toDirectoryState <$> scanCurrentDirectory

        render $ state
          { directoryState = directoryState
          , cursorPosition = (0, 1)
          }


  where
    fileList
      = files (directoryState state)

renderTree :: State -> Buffer -> Buffer
renderTree state buffer
  = foldr go buffer (enum $ toList fileList)
  where
    fileList
      = files (directoryState state)

    go (row, file)
      = printString
          (2, row)
          (fileStyle file)
          (filePath file)

drawCursor :: State -> Buffer -> Buffer
drawCursor state buffer
  = foldr go buffer [1..width]
  where
    (cursorCol, cursorRow)
      = cursorPosition state

    (width, _)
      = windowSize state

    go col
      = alterBuffer
          (col, cursorRow)
          (Termbox.Cell ' ' Termbox.underline mempty)

renderBuffer :: Buffer -> IO ()
renderBuffer buffer
  = for_ (Map.toList buffer) $ \((x, y), cell) ->
      Termbox.set x y cell

alterBuffer :: Point -> Termbox.Cell -> Buffer -> Buffer
alterBuffer point (Termbox.Cell c fg bg)
  = Map.alter modify point
  where
    modify (Just (Termbox.Cell _ fg' bg'))
      = Just $ Termbox.Cell c (fg <> fg') (bg <> bg')

    modify Nothing
      = Just $ Termbox.Cell c fg bg

cell
  :: Char
  -> (Termbox.Attr, Termbox.Attr)
  -> Termbox.Cell
cell c (fg, bg)
  = Termbox.Cell c fg bg

enum :: [a] -> [(Int, a)]
enum value = zipWith (,) [1..] value

printString
  :: Point
  -> (Termbox.Attr, Termbox.Attr)
  -> String
  -> Buffer
  -> Buffer
printString point@(col, row) style value buffer
  = case value of
      [] ->
        buffer
      x : xs ->
        printString (col + 1, row) style xs
        $ alterBuffer point (cell x style) buffer

fileStyle :: File -> (Termbox.Attr, Termbox.Attr)
fileStyle file
  = case fileType file of
      NormalFile -> (mempty, mempty)
      Folder     -> (Termbox.cyan, mempty)
