module Example.Monad.QexExample
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

filterAxiom1 :: FuncDecl -> AST -> Z3 ()
filterAxiom1 fn nil = do
  rs <- mkApp fn [nil]
  assert =<< mkEq rs nil

filterAxiom2 :: FuncDecl -> FuncDecl -> Sort -> Sort -> (AST -> Z3 AST) -> Z3 ()
filterAxiom2 fn cons x0Sort x1Sort condF = do
  x0 <- mkBound 1 x0Sort
  x1 <- mkBound 0 x1Sort
  filteredX1 <- mkApp fn [x1]
  trueBr <- mkApp cons [x0, filteredX1]
  falseBr <- mkApp fn [x1]
  cond <- condF x0
  rhs <- mkIte cond trueBr falseBr
  lhs <- mkApp cons [x0, x1]
  patAst <- mkApp fn [lhs]
  pat <- mkPattern [patAst]
  names <- mapM mkIntSymbol [0,1]
  axiom <- mkForall [pat] names [x0Sort, x1Sort] =<< mkEq patAst rhs
  axiomStr <- astToString axiom
  liftIO $ print axiomStr
  assert axiom

script :: Z3 String
script = do
  intSort <- mkIntSort
  stringSort <- mkStringSort
  studentSym <- mkStringSymbol "Student"
  studentNrSym <- mkStringSymbol "StudentNr"
  studentNameSym <- mkStringSymbol "StudentName"
  scoreSym <- mkStringSymbol "Score"
  studentIDSym <- mkStringSymbol "StudentID"
  courseIDSym <- mkStringSymbol "CourseID"
  pointsSym <- mkStringSymbol "Points"
  studentsSym <- mkStringSymbol "Students"
  scoresSym <- mkStringSymbol "Scores"
  studentScoreSym <- mkStringSymbol "StudentScore"
  studentScoreTableSym <- mkStringSymbol "StudentScoreTable"
  filter1Sym <- mkStringSymbol "filter1"

  (studentSort, _, [nrProj, nameProj]) <- mkTupleSort studentSym
    $ [(studentNrSym, intSort),
       (studentNameSym, stringSort)]

  (scoreSort, _, [sIDProj, cIDProj, ptsProj]) <- mkTupleSort scoreSym
    $ [(studentIDSym, intSort),
       (courseIDSym, intSort),
       (pointsSym, intSort)]

  -- table creation
  (studentsSort, studentsProjsFds) <- mkListSort studentsSym studentSort
  (scoresSort, scoresProjsFds) <- mkListSort scoresSym scoreSort

  -- join Scores and Students table
  (joinSort, _, [studentProj, scoreProj]) <- mkTupleSort studentScoreSym
    $ [(studentSym, studentSort),
       (scoreSym, scoreSort)]

  (joinTableSort, [jtNilFd, jtIsNilFd, jtConsFd, jtIsConsFd, jtHeadFd, jtTailFd]) <- mkListSort studentScoreTableSym joinSort

  -- disable MBQI
  mbqiParam <- mkParams
  mbqiSym <- mkStringSymbol "mbqi"
  paramsSetBool mbqiParam mbqiSym False
  solverSetParams mbqiParam

  -- 1st filter
  filter1 <- mkFuncDecl filter1Sym [joinTableSort] joinTableSort

  -- filter axioms
  jtNil <- mkApp jtNilFd []
  filterAxiom1 filter1 jtNil
  filterAxiom2 filter1 jtConsFd joinSort joinTableSort $
    (\x0 -> do
        student <- mkApp studentProj [x0]
        sNr <- mkApp nrProj [student]
        score <- mkApp scoreProj [x0]
        sID <- mkApp sIDProj [score]
        mkEq sNr sID)

  (_res, mbModel) <- getModel
  case mbModel of
    Just model -> showModel model
    Nothing    -> return "Couldn't construct model"
