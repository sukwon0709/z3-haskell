module Example.Monad.String
  ( run )
  where

import           Z3.Monad

run :: IO ()
run = evalZ3 script >>= putStrLn

script :: Z3 String
script = do
  stringSort <- mkStringSort
  s1Sym <- mkStringSymbol "s1"
  s1 <- mkConst s1Sym stringSort

  s2Sym <- mkStringSymbol "s2"
  s2 <- mkConst s2Sym stringSort

  s3 <- mkSeqConcat [s1, s2]
  assert =<< mkEq s3 =<< mkString "abcde"

  (_res, mbModel) <- getModel
  case mbModel of
    Just model -> showModel model
    Nothing    -> return "Couldn't construct model"
