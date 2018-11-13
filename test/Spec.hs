{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Relude

import qualified Data.List.PointedList as PointedList

import qualified Core

main :: IO ()
main = do
  searchTests

-- TODO use a proper framework :P
searchTests :: IO ()
searchTests = do
  let Just files
        = PointedList.fromList
            [ dummyFile "kangaroo"
            , dummyFile "panda"
            , dummyFile "giraffe"
            , dummyFile "elephant"
            , dummyFile "kitten"
            ]

  searchAndAssert files 0 "k" 5
  putTextLn (show files)
  putTextLn "yo"

dummyFile :: FilePath -> Core.File
dummyFile path
  = Core.File path Core.NormalFile

newtype IndexFocus = IndexFocus Int deriving Num
newtype ExpectedFocus = ExpectedFocus Int deriving Num

searchAndAssert
  :: Core.FilesList
  -> IndexFocus
  -> String
  -> ExpectedFocus
  -> IO ()
searchAndAssert files (IndexFocus index) search (ExpectedFocus expected)
  = if PointedList.index result == expected
      then pure ()
      else error "Failed blah"
  where
    Just newFiles
      = PointedList.moveTo index files

    result
      = Core.selectNextSearchMatch
          (Core.SearchPattern search)
          newFiles
