module Example.Monad.SMTLIB2
  ( run )
  where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Z3.Monad

run :: IO ()
run =
  let opts = (opt "auto_config" "false") +? (opt "timeout" "2000")
  in evalZ3With Nothing opts script >>= putStrLn

script :: Z3 String
script = do
  mbqiParam <- mkParams
  mbqiSym <- mkStringSymbol "mbqi"
  paramsSetBool mbqiParam mbqiSym False
  solverSetParams mbqiParam

  solverPush

  ast <- parseSMTLib2File "/tmp/z3.smt" [] [] [] []
  astStr <- astToString ast
  liftIO . putStrLn $ "AST: " ++ astStr

  assert ast

  (r, m) <- getModel
  case m of
    Just m' -> showModel m'
    Nothing -> return "Couldn't construct model"
