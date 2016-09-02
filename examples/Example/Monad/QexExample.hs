module Example.Monad.QexExample
  ( run )
  where

import           Z3.Monad

run :: IO ()
run = evalZ3 script >>= putStrLn

script :: Z3 String
script = do
  intSort <- mkIntSort
  stringSort <- mkStringSort
  scoreSym <- mkStringSymbol "Score"
  sIDSym <- mkStringSymbol "StudentID"
  cIDSym <- mkStringSymbol "CourseID"
  pSym <- mkStringSymbol "Points"
  (scoreSort, _scoreConstr, [scoreProj1, scoreProj2, scoreProj3]) <-
    mkTupleSort scoreSym [
    (sIDSym, intSort),
    (cIDSym, intSort),
    (pSym, intSort)
    ]
  studentSym <- mkStringSymbol "Student"
  sNrSym <- mkStringSymbol "StudentNr"
  sNameSym <- mkStringSymbol "StudentName"
  (studentSort, _studentConstr, [studentProj1, studentProj2]) <-
    mkTupleSort studentSym [
    (sNrSym, intSort),
    (sNameSym, stringSort)
    ]
  return "ABC"
