module FileManager where

import Relude hiding (State)
import qualified System.Directory as Directory
import qualified System.Posix.Files as Files
import qualified Termbox as Termbox

import qualified Debug.Trace as Debug

data FileType
  = NormalFile
  | Folder
  deriving (Show)

data File
  = File
      { path :: FilePath
      , fileType :: FileType
      }
  deriving (Show)

data State
  = State
      { files :: [File]
      , currentPath :: FilePath
      }
  deriving (Show)

run :: IO ()
run = do
  let currentPath = "."

  dirs <- Directory.listDirectory currentPath
  files <- forM dirs $ \path -> do
    status <- Files.getFileStatus path
    pure File
          { path = path
          , fileType
              = if Files.isRegularFile status
                  then NormalFile
                  else Folder
          }

  let initialState
        = State
            { files = files
            , currentPath = currentPath
            }

  Termbox.main $ renderTree initialState

renderTree :: State -> IO ()
renderTree state = do
  Termbox.clear mempty mempty
  Termbox.hideCursor

  for_ (enum $ files state) $ \(row, file) ->
    printString (2, row) (mempty, mempty) (path file)


  Termbox.flush

  event <- Termbox.poll
  pure ()




cell
  :: Char
  -> Termbox.Attr
  -> Termbox.Attr
  -> Termbox.Cell
cell
  = Termbox.Cell

enum :: [a] -> [(Int, a)]
enum value = zipWith (,) [1..] value

printString
  :: (Int, Int)
  -> (Termbox.Attr, Termbox.Attr)
  -> String
  -> IO ()
printString (col, row) (fg, bg) value
  = for_ (enum value) $ \(index, char) ->
      Termbox.set
        (col + index)
        row
        (cell char fg bg)
