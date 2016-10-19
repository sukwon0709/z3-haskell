
module Main where

import qualified Example.Monad.DataTypes
import qualified Example.Monad.FuncModel
import qualified Example.Monad.Interpolation
import qualified Example.Monad.List
import qualified Example.Monad.QexExample
import qualified Example.Monad.Queens4
import qualified Example.Monad.Queens4All
import qualified Example.Monad.SMTLIB2
import qualified Example.Monad.String
import qualified Example.Monad.ToSMTLib
import qualified Example.Monad.Tuple

import           System.Environment

examples =
  [ ("4queens"
    , Example.Monad.Queens4.run
    )
  , ("all4queens"
     , Example.Monad.Queens4All.run
     )
  , ("datatypes"
    , Example.Monad.DataTypes.run
    )
  , ("funcmodel"
    , Example.Monad.FuncModel.run
    )
  , ("smtlib"
    , Example.Monad.ToSMTLib.run
    )
  , ("tuple"
    , Example.Monad.Tuple.run
    )
  , ("interpolation"
    , Example.Monad.Interpolation.run
    )
  , ("string"
    , Example.Monad.String.run
    )
  , ("qexexample"
    , Example.Monad.QexExample.run
    )
  , ("list"
    , Example.Monad.List.run
    )
  , ("smtlib2"
    , Example.Monad.SMTLIB2.run
    )
  ]

runExample :: String -> IO ()
runExample x = case lookup x examples of
                    Just x  -> x
                    Nothing -> error "Example not found"

listExamples :: IO ()
listExamples = mapM_ (putStrLn . fst) examples

main = do
  args <- getArgs
  if null args
    then listExamples
    else case args !! 0 of
              x      -> runExample x
