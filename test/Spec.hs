{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Relude
import Test.Hspec

import qualified Data.List.PointedList as PointedList

import qualified Core

newtype Dummy a = Dummy { runApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Core.FileSystem Dummy where
    scanDirectory _ = pure files

    resolvePath "/usr/bin/.."  = pure "/usr"
    resolvePath a              = pure a

    homeDirectoryPath = pure "/home/user"

files :: PointedList.PointedList Core.File
files =
        let
            Just f =
                   PointedList.fromList
                    [ dummyFile "kangaroo"
                    , dummyFile "panda"
                    , dummyFile "giraffe"
                    , dummyFile "elephant"
                    , dummyFile "kitten"
                    ]

        in
            f

main :: IO ()
main = hspec $ do
  describe "Search" $ do

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

    describe "update" $ do
        let dummyState = Core.State
                         { Core.files = files
                           , Core.currentPath = "/usr/bin/"
                           , Core.originalPath = "/"
                           , Core.homeDirectory = "/home/user"
                           , Core.currentMode = Core.ModeNavigation
                           , Core.searchPattern = Core.SearchPattern ""
                         }

        it "JumpNext" $ do
            Core.Running agg <- runApp $ Core.update dummyState Core.JumpNext
            let result = PointedList._focus (Core.files agg)
            result `shouldBe` dummyFile "panda"

        it "JumpPrev" $ do
            _ <- runApp $ Core.update dummyState Core.JumpNext
            Core.Running agg <- runApp $ Core.update dummyState Core.JumpPrev

            let result = PointedList._focus (Core.files agg)
            result `shouldBe` dummyFile "kangaroo"

        it "JumpMany" $ do
            Core.Running agg <- runApp $ Core.update dummyState $ Core.JumpMany 2

            let result = PointedList._focus (Core.files agg)
            result `shouldBe` dummyFile "giraffe"

        it "JumpParentFolder" $ do
            Core.Running newState <- runApp $ Core.update dummyState Core.JumpParentFolder

            Core.currentPath newState `shouldBe` "/usr"

        it "JumpHomeFolder" $ do
            Core.Running newState <- runApp $ Core.update dummyState Core.JumpHomeDirectory

            Core.currentPath newState `shouldBe` "/home/user"

        it "JumpBeginning" $ do
            _ <- runApp $ Core.update dummyState Core.JumpNext
            Core.Running agg <- runApp $ Core.update dummyState Core.JumpBeginning

            let result = PointedList._focus (Core.files agg)
            result `shouldBe` dummyFile "kangaroo"

        it "JumpEnd" $ do
            Core.Running agg <- runApp $ Core.update dummyState Core.JumpEnd

            let result = PointedList._focus (Core.files agg)
            result `shouldBe` dummyFile "kitten"

        it "SelectCurrentFile" $ do
            _ <- runApp $ Core.update dummyState Core.JumpNext
            Core.FileSelected agg <- runApp $ Core.update dummyState Core.SelectCurrentFile

            agg `shouldBe` "/usr/bin/kangaroo"

        it "SwitchMode" $ do
            Core.Running agg <- runApp $ Core.update dummyState (Core.SwitchMode Core.ModeSearch)
            Core.currentMode agg `shouldBe` Core.ModeSearch

        it "UpdateSearch" $ do
            Core.Running agg <- runApp $ Core.update dummyState (Core.UpdateSearch $ Core.SearchPattern "AdventOfCode")
            Core.searchPattern agg `shouldBe` Core.SearchPattern "AdventOfCode"

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
searchAndAssert movement (IndexFocus index) search (ExpectedPath expected) fileList
  = foundPath `shouldBe` expected
  where
    foundPath
      = Core.filePath $ PointedList._focus result

    Just newFiles
      = PointedList.moveTo index fileList

    result
      = Core.searchNext movement search newFiles
