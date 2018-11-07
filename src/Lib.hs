module Lib
    ( run
    ) where


import Relude hiding (State)
import qualified Data.Text as Text
import qualified Termbox as Termbox


run :: IO ()
run
  -- = Termbox.main $ loopPrint "yo"
  = mainRead


loopPrint :: String -> IO ()
loopPrint value = do
  Termbox.clear Termbox.red Termbox.yellow
  Termbox.hideCursor

  -- Termbox.setInputMode
    -- (Termbox.InputModeAlt Termbox.MouseModeNo)

  for_ (enum value) $ \(index, char) -> do
    Termbox.set
      (3 + index)
      3
      (cell char mempty mempty)

  Termbox.flush

  event <- Termbox.poll
  -- traceShowM $ show event

  case event of
    Termbox.EventKey key _ ->
      handleKey key value

    Termbox.EventResize width height ->
      loopPrint value

    Termbox.EventMouse _ width height ->
      loopPrint value


handleKey :: Termbox.Key -> String -> IO ()
handleKey key value
  = case key of
      Termbox.KeyChar char ->
        loopPrint (value <> [char])

      Termbox.KeySpace ->
        loopPrint (value <> " ")

      Termbox.KeyBackspace ->
        let len = (length value) - 1
        in loopPrint (take len value)

      Termbox.KeyCtrlC -> pure ()

      _ -> loopPrint (show key)

-----------


data State
  = State
      { file   :: Text
      , offset :: (Int, Int)
      }

mainRead :: IO ()
mainRead = do
  content <- readFile "../co-log/co-log/README.md"

  let initialState
        = State
            { file = content
            , offset = (0, 0)
            }

  Termbox.main $ loopRead initialState

loopRead :: State -> IO ()
loopRead state = do
  Termbox.clear Termbox.red Termbox.yellow
  Termbox.hideCursor

  let content = contentFromState state
  drawFile content
  drawNavBar state

  Termbox.flush

  event <- Termbox.poll
  handleEvent event state


drawFile :: [String] -> IO ()
drawFile content = do
  for_ (enum content) $ \(row, line) ->
    printString (0, row) (mempty, mempty) line


contentFromState :: State -> [String]
contentFromState state
  =   (drop x . toString)
  <$> (drop y content)
  where
    content
      = Text.lines (file state)

    (x, y)
      = offset state


handleEvent :: Termbox.Event -> State -> IO ()
handleEvent ev state
  = case ev of
      Termbox.EventKey Termbox.KeyCtrlC _ ->
        pure ()

      Termbox.EventKey (Termbox.KeyChar key) _  ->
        case key of
          'l' -> loopRead (setOffset state   1   0)
          'h' -> loopRead (setOffset state (-1)  0)
          'j' -> loopRead (setOffset state   0   1)
          'k' -> loopRead (setOffset state   0 (-1))
          _   -> loopRead state

      _ ->
        loopRead state

setOffset :: State -> Int -> Int -> State
setOffset state x y
  = state { offset = (newX, newY) }
  where
    (oldX, oldY)
      = offset state

    newX
      = max 0 (oldX + x)

    newY
      = max 0 (oldY + y)

drawNavBar :: State -> IO ()
drawNavBar state = do
  (width, height) <- Termbox.size
  printString
    (width - 10, height - 2)
    (Termbox.black, Termbox.green)
    (show $ offset state)

cell
  :: Char
  -> Termbox.Attr
  -> Termbox.Attr
  -> Termbox.Cell
cell
  = Termbox.Cell


enum :: [a] -> [(Int, a)]
enum value = zipWith (,) [1..] value

isCtrlC :: Termbox.Event -> Bool
isCtrlC ev
  = case ev of
      Termbox.EventKey Termbox.KeyCtrlC _ -> True
      _                                   -> False

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
