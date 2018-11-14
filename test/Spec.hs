{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Relude
import Test.Hspec

import qualified Data.List.PointedList as PointedList

import qualified Core

main :: IO ()
main = hspec $ do
  describe "Search" $ do

    let Just files
          = PointedList.fromList
              [ dummyFile "kangaroo"
              , dummyFile "panda"
              , dummyFile "giraffe"
              , dummyFile "elephant"
              , dummyFile "kitten"
              ]

    it "forward search" $ do

      searchAndAssert
        Core.Forward
        (IndexFocus 0)
        (Core.SearchPattern "k")
        (ExpectedPath "kitten")
        files

      searchAndAssert
        Core.Forward
        (IndexFocus 2)
        (Core.SearchPattern "an")
        (ExpectedPath "elephant")
        files

      searchAndAssert
        Core.Forward
        (IndexFocus 4)
        (Core.SearchPattern "an")
        (ExpectedPath "kangaroo")
        files

      searchAndAssert
        Core.Forward
        (IndexFocus 4)
        (Core.SearchPattern "GARO")
        (ExpectedPath "kangaroo")
        files

      searchAndAssert
        Core.Forward
        (IndexFocus 3)
        (Core.SearchPattern "pand")
        (ExpectedPath "panda")
        files

    it "backward search" $ do

      searchAndAssert
        Core.Backward
        (IndexFocus 0)
        (Core.SearchPattern "k")
        (ExpectedPath "kitten")
        files

      searchAndAssert
        Core.Backward
        (IndexFocus 2)
        (Core.SearchPattern "anga")
        (ExpectedPath "kangaroo")
        files

      searchAndAssert
        Core.Backward
        (IndexFocus 4)
        (Core.SearchPattern "PAND")
        (ExpectedPath "panda")
        files

      searchAndAssert
        Core.Backward
        (IndexFocus 2)
        (Core.SearchPattern "ten")
        (ExpectedPath "kitten")
        files

      searchAndAssert
        Core.Backward
        (IndexFocus 1)
        (Core.SearchPattern "gira")
        (ExpectedPath "giraffe")
        files

dummyFile :: FilePath -> Core.File
dummyFile path
  = Core.File path Core.NormalFile

newtype IndexFocus = IndexFocus Int
newtype ExpectedPath = ExpectedPath FilePath

searchAndAssert
  :: Core.Movement
  -> IndexFocus
  -> Core.SearchPattern
  -> ExpectedPath
  -> Core.FilesList
  -> IO ()
searchAndAssert movement (IndexFocus index) search (ExpectedPath expected) files
  = foundPath `shouldBe` expected
  where
    foundPath
      = Core.filePath $ PointedList._focus result

    Just newFiles
      = PointedList.moveTo index files

    result
      = Core.searchNext movement search newFiles
