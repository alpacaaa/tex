module Buffer where

import           Relude hiding (state)

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Termbox

import qualified Core


data Event
  = Quit
  | UnrecognizedInput Termbox.Event
  | AppCmd Core.Cmd

type Point
  = (Int, Int)

type Buffer
  = Map Point Termbox.Cell

data Env
  = Env
      { windowSize   :: (Int, Int)
      , bufferOffset :: Int
      }

run :: IO a -> IO a
run = Termbox.main

render :: Core.State -> IO Event
render state = do
  Termbox.clear mempty mempty
  Termbox.hideCursor

  size <- Termbox.size
  let offset= determineOffset size state

      env
       = Env
          { windowSize   = size
          , bufferOffset = offset
          }

      mainBuffer
        = drawCursor env state mempty
        & renderTree env state

      folderInfo
        = drawFolderInfo env state mempty

  renderBuffer folderInfo (2, 1)
  renderBuffer mainBuffer (2, 2)

  Termbox.flush

  event <- Termbox.poll
  pure (handleEvent event)

handleEvent :: Termbox.Event -> Event
handleEvent ev
  = case ev of
      Termbox.EventKey Termbox.KeyCtrlC _ ->
        Quit

      Termbox.EventKey Termbox.KeyEnter _ ->
        AppCmd Core.SelectCurrentFile

      Termbox.EventKey (Termbox.KeyChar key) _  ->
        case key of
          'j' -> AppCmd Core.JumpNext
          'k' -> AppCmd Core.JumpPrev
          '-' -> AppCmd Core.JumpParentFolder
          '~' -> AppCmd Core.JumpHomeDirectory
          'g' -> AppCmd Core.JumpBeginning
          'G' -> AppCmd Core.JumpEnd
          'q' -> Quit
          _   -> UnrecognizedInput ev

      Termbox.EventKey Termbox.KeyArrowDown _ ->
        AppCmd Core.JumpNext

      Termbox.EventKey Termbox.KeyArrowUp _ ->
        AppCmd Core.JumpPrev

      Termbox.EventKey Termbox.KeyCtrlD _ ->
        AppCmd (Core.JumpMany 17)

      Termbox.EventKey Termbox.KeyCtrlU _ ->
        AppCmd (Core.JumpMany $ -17)

      Termbox.EventKey Termbox.KeyCtrlF _ ->
        AppCmd (Core.JumpMany 34)

      Termbox.EventKey Termbox.KeyCtrlB _ ->
        AppCmd (Core.JumpMany $ -34)

      _ ->
        UnrecognizedInput ev

renderTree :: Env -> Core.State -> Buffer -> Buffer
renderTree env state buffer
  = foldr go buffer files
  where
    files
      = Core.files state
        & toList
        & drop (bufferOffset env)
        & enum

    go (row, file)
      = printString
          (0, row)
          (fileStyle file)
          (Core.fileDisplayName file)

drawCursor :: Env -> Core.State -> Buffer -> Buffer
drawCursor env state buffer
  = foldr go buffer [0..width]
  where
    cursorRow
      = cursorPosition env state

    (width, _)
      = windowSize env

    go col
      = alterBuffer
          (col, cursorRow)
          (Termbox.Cell ' ' Termbox.underline mempty)

drawFolderInfo :: Env -> Core.State -> Buffer -> Buffer
drawFolderInfo _ state buffer
  = printString
      (0, 0)
      (Termbox.green <> Termbox.bold, mempty)
      homeStripped
      buffer
  where
    -- a bit brutal
    homeStripped
      = case List.stripPrefix home path of
          Just suffix -> "~" <> suffix
          Nothing     -> path

    home = Core.homeDirectory state
    path = Core.currentPath state

renderBuffer :: Buffer -> (Int, Int) -> IO ()
renderBuffer buffer (offsetX, offsetY)
  = for_ (Map.toList buffer) $ \((x, y), cell) ->
      Termbox.set (offsetX + x) (offsetY + y) cell

alterBuffer :: Point -> Termbox.Cell -> Buffer -> Buffer
alterBuffer point (Termbox.Cell c fg bg)
  = Map.alter go point
  where
    go (Just (Termbox.Cell _ fg' bg'))
      = Just $ Termbox.Cell c (fg <> fg') (bg <> bg')

    go Nothing
      = Just $ Termbox.Cell c fg bg

buildCell
  :: Char
  -> (Termbox.Attr, Termbox.Attr)
  -> Termbox.Cell
buildCell c (fg, bg)
  = Termbox.Cell c fg bg

enum :: [a] -> [(Int, a)]
enum value = zipWith (,) [0..] value

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
        $ alterBuffer point (buildCell x style) buffer

fileStyle :: Core.File -> (Termbox.Attr, Termbox.Attr)
fileStyle file
  = case Core.fileType file of
      Core.NormalFile      -> (mempty, mempty)
      Core.SymbolicLink _  -> (Termbox.magenta, mempty)
      Core.Folder          -> (Termbox.blue <> Termbox.bold, mempty)

cursorPosition :: Env -> Core.State -> Int
cursorPosition env state
  = if offset == 0
      then index
      else index - offset
  where
    index
      = Core.currentIndex state

    offset
      = bufferOffset env

determineOffset :: (Int, Int) -> Core.State -> Int
determineOffset (_, height) state
  = if index < threshold
      then 0
      else index - threshold
  where
    index
      = Core.currentIndex state

    threshold
      = round (0.75 * fromIntegral height :: Double)
