module CLI where

import           Relude hiding (state)

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Termbox

import qualified Core

{-
  This module does a lot of IO, it's pretty concrete and it's not necessarely
  interesting from an architecture point of view.

  The main idea is that when the CLI is running, it will emit some events of
  type `Event`. Certain events correlate directly with a `Core.Cmd` and we can
  use them to advance the state of the application.
-}

-- A CLI event.
data Event
  = Quit
  | UnrecognizedInput Termbox.Event
  | AppCmd Core.Cmd

-- | The screen is basically a 2d grid.
type Point
  = (Int, Int)

-- | Instead of writing directly to the screen, we use this data structure to
-- buffer all the changes we'd like to render in this frame. A `Termbox.Cell`
-- represents one character on the screen, with its foreground and background
-- colors.
type Buffer
  = Map Point Termbox.Cell

data Env
  = Env
      { windowSize   :: (Int, Int)
      -- | how far down the screen the selected file is
      , bufferOffset :: Int
      }

run :: IO a -> IO a
run = Termbox.main

-- | Every time the state is updated, a re-render is needed to update the
-- screen. We then wait for an event and return it.
render :: Core.State -> IO Event
render state = do
  Termbox.clear mempty mempty
  Termbox.hideCursor

  size <- Termbox.size
  let offset
        = determineOffset size state

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

      searchInput
        = case Core.currentMode state of
            Core.ModeNavigation -> mempty
            Core.ModeSearch -> drawSearchInput env state mempty

  renderBuffer folderInfo (2, 1)
  renderBuffer mainBuffer (2, 2)
  renderBuffer searchInput (0, 0)

  Termbox.flush

  event <- Termbox.poll
  case Core.currentMode state of
    Core.ModeNavigation ->
      pure (handleNavigationEvent event)
    Core.ModeSearch ->
      let searchPattern = Core.searchPattern state
      in  pure (handleSearchEvent searchPattern event)

handleNavigationEvent :: Termbox.Event -> Event
handleNavigationEvent ev
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
          '/' -> AppCmd $ Core.SwitchSearchMode Core.Forward
          '?' -> AppCmd $ Core.SwitchSearchMode Core.Backward
          'n' -> AppCmd Core.SearchNextMatch
          'N' -> AppCmd Core.SearchPrevMatch
          'q' -> Quit
          'u' -> AppCmd Core.Undo
          'U' -> AppCmd Core.Redo
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

handleSearchEvent :: Core.SearchPattern -> Termbox.Event -> Event
handleSearchEvent (Core.SearchPattern searchPattern) ev
  = case ev of
      Termbox.EventKey Termbox.KeyCtrlC _ ->
        AppCmd $ Core.SwitchMode Core.ModeNavigation

      Termbox.EventKey Termbox.KeyEsc _ ->
        AppCmd $ Core.SwitchMode Core.ModeNavigation

      Termbox.EventKey Termbox.KeyEnter _ ->
        AppCmd Core.CommitSearch

      Termbox.EventKey Termbox.KeyBackspace2 _ ->
        let
          -- All but the last element
          newPattern = zipWith const searchPattern $ drop 1 searchPattern
        in
        AppCmd
          $ Core.UpdateSearch
          $ Core.SearchPattern newPattern

      Termbox.EventKey (Termbox.KeyChar key) _  ->
        AppCmd
          $ Core.UpdateSearch
          $ Core.SearchPattern (searchPattern <> [key])

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
      = writeToBuffer
          (0, row)
          (fileStyle file)
          (Core.fileDisplayName file)

drawCursor :: Env -> Core.State -> Buffer -> Buffer
drawCursor env state buffer
  = applyStyleToRow env buffer cursorRow (Termbox.underline, mempty)
  where
    cursorRow
      = cursorPosition env state

drawFolderInfo :: Env -> Core.State -> Buffer -> Buffer
drawFolderInfo _ state buffer
  = writeToBuffer
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

drawSearchInput :: Env -> Core.State -> Buffer -> Buffer
drawSearchInput env state buffer
  = applyStyleToRow env buffer lastRow (mempty, mempty)
  & writeToBuffer
      (0, lastRow)
      (Termbox.bold <> Termbox.white, Termbox.cyan)
      "search:"
  & writeToBuffer
      (7, lastRow)
      (mempty, mempty)
      search
  where
    (_, height)
      = windowSize env

    lastRow
      = height - 1

    Core.SearchPattern search
      = Core.searchPattern state

-- | A `Buffer` is just a representation of what needs to be displayed.
-- This function renders the buffer content to the screen.
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

writeToBuffer
  :: Point
  -> (Termbox.Attr, Termbox.Attr)
  -> String
  -> Buffer
  -> Buffer
writeToBuffer point@(col, row) style value buffer
  = case value of
      [] ->
        buffer
      x : xs ->
        writeToBuffer (col + 1, row) style xs
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

-- | Will override all styles and text content.
applyStyleToRow :: Env -> Buffer -> Int -> (Termbox.Attr, Termbox.Attr) -> Buffer
applyStyleToRow env buffer row style
  = foldr go buffer [0..width]
  where
    go col
      = alterBuffer (col, row) (buildCell ' ' style)

    (width, _)
      = windowSize env
