module Buffer where

import Relude

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


render :: Core.Env -> Core.State -> IO Event
render env state = do
  Termbox.clear mempty mempty
  Termbox.hideCursor

  windowSize <- Termbox.size
  let newEnv
        = env { Core.windowSize = windowSize }

  let buffer
        = drawCursor env state mempty
        & renderTree env state

  renderBuffer buffer

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
          _   -> UnrecognizedInput ev

      Termbox.EventKey Termbox.KeyArrowDown _ ->
        AppCmd Core.JumpNext

      Termbox.EventKey Termbox.KeyArrowUp _ ->
        AppCmd Core.JumpPrev

      _ ->
        UnrecognizedInput ev


renderTree :: Core.Env -> Core.State -> Buffer -> Buffer
renderTree env state buffer
  = foldr go buffer (enum $ toList fileList)
  where
    fileList
      = Core.files state

    go (row, file)
      = printString
          (2, row)
          (fileStyle file)
          (Core.filePath file)

drawCursor :: Core.Env -> Core.State -> Buffer -> Buffer
drawCursor env state buffer
  = foldr go buffer [1..width]
  where
    (cursorCol, cursorRow)
      = cursorPosition state

    (width, _)
      = Core.windowSize env

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

fileStyle :: Core.File -> (Termbox.Attr, Termbox.Attr)
fileStyle file
  = case Core.fileType file of
      Core.NormalFile -> (mempty, mempty)
      Core.Folder     -> (Termbox.cyan, mempty)

cursorPosition :: Core.State -> (Int, Int)
cursorPosition = undefined
