module Example.Monad.List
  ( run )
  where

import           Z3.Monad

run :: IO ()
run = evalZ3 script >>= putStrLn

script :: Z3 String
script = do
  lName <- mkStringSymbol "IntList"
  intSort <- mkIntSort
  (lSort, [nilFd, isNilFd, consFd, isConsFd, headFd, tailFd]) <-
    mkListSort lName intSort
  lSym <- mkStringSymbol "l"
  l <- mkConst lSym lSort
  p <- mkApp nilFd []
  assert =<< mkNot =<< mkEq l p
  (_res, mbModel) <- getModel
  case mbModel of
    Just model -> showModel model
    Nothing    -> return "Couldn't construct model"
