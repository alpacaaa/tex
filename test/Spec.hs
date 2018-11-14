{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Relude
import Test.Hspec

import qualified Data.List.PointedList as PointedList

import qualified Core

main :: IO ()
main = hspec $ do
  describe "Search" $ do
    it "forward search" $ do
      let Just files
            = PointedList.fromList
                [ dummyFile "kangaroo"
                , dummyFile "panda"
                , dummyFile "giraffe"
                , dummyFile "elephant"
                , dummyFile "kitten"
                ]

      searchAndAssert
        files
        (IndexFocus 0)
        (Core.SearchPattern "k")
        (ExpectedPath "kitten")

      searchAndAssert
        files
        (IndexFocus 2)
        (Core.SearchPattern "an")
        (ExpectedPath "elephant")

      searchAndAssert
        files
        (IndexFocus 4)
        (Core.SearchPattern "an")
        (ExpectedPath "kangaroo")

      searchAndAssert
        files
        (IndexFocus 4)
        (Core.SearchPattern "GARO")
        (ExpectedPath "kangaroo")

      searchAndAssert
        files
        (IndexFocus 3)
        (Core.SearchPattern "pand")
        (ExpectedPath "panda")

dummyFile :: FilePath -> Core.File
dummyFile path
  = Core.File path Core.NormalFile

newtype IndexFocus = IndexFocus Int
newtype ExpectedPath = ExpectedPath FilePath

searchAndAssert
  :: Core.FilesList
  -> IndexFocus
  -> Core.SearchPattern
  -> ExpectedPath
  -> IO ()
searchAndAssert files (IndexFocus index) search (ExpectedPath expected)
  = foundPath `shouldBe` expected
  where
    foundPath
      = Core.filePath $ PointedList._focus result

    Just newFiles
      = PointedList.moveTo index files

    result
      = Core.selectNextSearchMatch search newFiles
