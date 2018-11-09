{-# LANGUAGE NamedFieldPuns #-}

module FileManager where

import Relude hiding (State)
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map
import qualified System.Directory as Directory
import qualified System.Posix.Files as Files
import qualified Termbox as Termbox

import qualified Debug.Trace as Debug

data FileType
  = NormalFile
  | Folder
  deriving (Show, Eq, Ord)

data File
  = File
      { path :: FilePath
      , fileType :: FileType
      }
  deriving (Show)

data DirectoryState
  = DirectoryState
      { files :: [File]
      , currentPath :: FilePath
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

initApp :: IO State
initApp = do
  let currentPath = "."

  dirs <- Directory.getDirectoryContents currentPath
  files <- forM dirs $ \path -> do
    status <- Files.getFileStatus path
    pure File
          { path = path
          , fileType
              = if Files.isRegularFile status
                  then NormalFile
                  else Folder
          }

  let directoryState
        = DirectoryState
            { files = sortOn (Data.Ord.Down . fileType) files
            , currentPath = currentPath
            }

      initialState
        = State
            { directoryState = directoryState
            , cursorPosition = (0, 5)
            , windowSize = (0, 0)
            }

  pure initialState

run :: IO ()
run = do
  state <- initApp
  Termbox.main $ render state

render :: State -> IO ()
render state = do
  Termbox.clear mempty mempty
  windowSize <- Termbox.size
  Termbox.hideCursor

  let state' = state { windowSize = windowSize }

  let buffer
        = drawCursor state' mempty
        & renderTree state'

  renderBuffer buffer

  Termbox.flush

  event <- Termbox.poll
  handleEvent event state
  pure ()

handleEvent :: Termbox.Event -> State -> IO ()
handleEvent ev state@(State {cursorPosition = (x, y) })
  = case ev of
     Termbox.EventKey Termbox.KeyCtrlC _ ->
       pure ()

     Termbox.EventKey (Termbox.KeyChar key) _  ->
       case key of
         'j' -> moveDown
         'k' -> moveUp
         _   -> render state

     Termbox.EventKey Termbox.KeyArrowDown _ ->
       moveDown

     Termbox.EventKey Termbox.KeyArrowUp _ ->
       moveUp

     _ ->
       render state
  where
      moveDown = render $ state { cursorPosition = (x, y + 1) }
      moveUp = render $ state { cursorPosition = (x, y - 1) }

renderTree :: State -> Buffer -> Buffer
renderTree state buffer
  = foldr go buffer (enum fileList)
  where
    fileList
      = files (directoryState state)

    go (row, file)
      = case fileType file of
          NormalFile -> printString
                        (2, row)
                        (Termbox.yellow, mempty)
                        (path file)
          Folder     -> printString
                         (2, row)
                         (Termbox.cyan, mempty)
                         (path file)

drawCursor :: State -> Buffer -> Buffer
drawCursor state@(State {windowSize = (width, _)}) buffer
  = foldr go buffer [1..width]
  where
    (cursorCol, cursorRow)
      = cursorPosition state

    go col
      = alterBuffer
          (col, cursorRow)
          (Termbox.Cell ' ' Termbox.red Termbox.green)

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

{-
  = for_ (enum value) $ \(index, char) ->
      Termbox.set
        (col + index)
        row
        (cell char fg bg)
-}
