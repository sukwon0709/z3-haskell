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
  liftIO $ putStrLn axiomStr
  assert axiom

crossAxiom1 :: FuncDecl -> Sort -> AST -> AST -> Z3 ()
crossAxiom1 fn t2Sort t1Nil jNil = do
  x <- mkBound 0 t2Sort
  lhs <- mkApp fn [t1Nil, x]
  pat <- mkPattern [lhs]
  name <- mkIntSymbol 0
  axiom <- mkForall [pat] [name] [t2Sort] =<< mkEq lhs jNil
  axiomStr <- astToString axiom
  liftIO $ putStrLn axiomStr
  assert axiom

crossAxiom2 :: FuncDecl -> Sort -> AST -> AST -> Z3 ()
crossAxiom2 fn t1Sort t2Nil jNil = do
  x <- mkBound 0 t1Sort
  lhs <- mkApp fn [x, t2Nil]
  pat <- mkPattern [lhs]
  name <- mkIntSymbol 0
  axiom <- mkForall [pat] [name] [t1Sort] =<< mkEq lhs jNil
  axiomStr <- astToString axiom
  liftIO $ putStrLn axiomStr
  assert axiom

crossAxiom3 :: FuncDecl
            -> FuncDecl
            -> FuncDecl
            -> FuncDecl
            -> Sort
            -> Sort
            -> Sort
            -> Sort
            -> Z3 ()
crossAxiom3 crossFn crFn t1ConsFd t2ConsFd r1Sort t1Sort r2Sort t2Sort = do
  x0 <- mkBound 3 r1Sort
  x1 <- mkBound 2 t1Sort
  x2 <- mkBound 1 r2Sort
  x3 <- mkBound 0 t2Sort
  t1 <- mkApp t1ConsFd [x0, x1]
  t2 <- mkApp t2ConsFd [x2, x3]
  lhs <- mkApp crossFn [t1, t2]
  rhs <- mkApp crFn [x0, x1, t2, t2]
  pat <- mkPattern [lhs]
  names <- mapM mkIntSymbol [0,1,2,3]
  axiom <- mkForall [pat] names [r1Sort, t1Sort, r2Sort, t2Sort] =<< mkEq lhs rhs
  axiomStr <- astToString axiom
  liftIO $ putStrLn axiomStr
  assert axiom

crossAxiom4 :: FuncDecl
            -> FuncDecl
            -> Sort
            -> Sort
            -> Sort
            -> AST
            -> Z3 ()
crossAxiom4 crossFn crFn r1Sort t1Sort t2Sort t2Nil = do
  x0 <- mkBound 2 r1Sort
  x1 <- mkBound 1 t1Sort
  x2 <- mkBound 0 t2Sort
  lhs <- mkApp crFn [x0, x1, t2Nil, x2]
  rhs <- mkApp crossFn [x1, x2]
  pat <- mkPattern [lhs]
  names <- mapM mkIntSymbol [0,1,2]
  axiom <- mkForall [pat] names [r1Sort, t1Sort, t2Sort] =<< mkEq lhs rhs
  axiomStr <- astToString axiom
  liftIO $ putStrLn axiomStr
  assert axiom

crossAxiom5 :: FuncDecl
            -> FuncDecl
            -> FuncDecl
            -> FuncDecl
            -> Sort
            -> Sort
            -> Sort
            -> Sort
            -> Z3 ()
crossAxiom5 crFn t2ConsFn jtConsFn tupFn r1Sort t1Sort r2Sort t2Sort = do
  x0 <- mkBound 4 r1Sort
  x1 <- mkBound 3 t1Sort
  x2 <- mkBound 2 r2Sort
  x3 <- mkBound 1 t2Sort
  x4 <- mkBound 0 t2Sort
  t1 <- mkApp t2ConsFn [x2, x3]
  lhs <- mkApp crFn [x0, x1, t1, x4]
  t2 <- mkApp tupFn [x0, x2]
  t3 <- mkApp crFn [x0, x1, x3, x4]
  rhs <- mkApp jtConsFn [t2, t3]
  pat <- mkPattern [lhs]
  names <- mapM mkIntSymbol [0,1,2,3,4]
  axiom <- mkForall [pat] names [r1Sort, t1Sort, r2Sort, t2Sort, t2Sort] =<< mkEq lhs rhs
  axiomStr <- astToString axiom
  liftIO $ putStrLn axiomStr
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
  filter2Sym <- mkStringSymbol "filter2"
  cross1Sym <- mkStringSymbol "cross1"
  cr1Sym <- mkStringSymbol "cr1"

  (studentSort, _, [nrProj, nameProj]) <- mkTupleSort studentSym
    $ [(studentNrSym, intSort),
       (studentNameSym, stringSort)]

  (scoreSort, _, [sIDProj, cIDProj, ptsProj]) <- mkTupleSort scoreSym
    $ [(studentIDSym, intSort),
       (courseIDSym, intSort),
       (pointsSym, intSort)]

  -- table creation
  (studentsSort, [studentsNilFd, _, studentsConsFd, _, studentsHeadFd, studentsTailFd]) <- mkListSort studentsSym studentSort
  (scoresSort, [scoresNilFd, _, scoresConsFd, _, scoresHeadFd, scoresTailFd]) <- mkListSort scoresSym scoreSort

  -- join Scores and Students table
  (joinSort, joinCtor, [studentProj, scoreProj]) <- mkTupleSort studentScoreSym
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

  -- 2nd filter
  filter2 <- mkFuncDecl filter2Sym [joinTableSort] joinTableSort
  filterAxiom1 filter2 jtNil
  filterAxiom2 filter2 jtConsFd joinSort joinTableSort $
    (\x0 -> do
        score <- mkApp scoreProj [x0]
        cID <- mkApp cIDProj [score]
        pts <- mkApp ptsProj [score]
        c1 <- mkEq cID =<< mkIntNum (10 :: Integer)
        c2 <- mkGt pts =<< mkIntNum (0 :: Integer)
        mkAnd [c1, c2])

  -- cross axiom1,2
  cross1 <- mkFuncDecl cross1Sym [studentsSort, scoresSort] joinTableSort
  studentsNil <- mkApp studentsNilFd []
  crossAxiom1 cross1 scoresSort studentsNil jtNil
  scoresNil <- mkApp scoresNilFd []
  crossAxiom2 cross1 studentsSort scoresNil jtNil

  -- cross axiom3
  cr1 <- mkFuncDecl cr1Sym [studentSort, studentsSort, scoresSort, scoresSort] joinTableSort
  crossAxiom3 cross1 cr1 studentsConsFd scoresConsFd studentSort studentsSort scoreSort scoresSort

  -- cross axiom4
  crossAxiom4 cross1 cr1 studentSort studentsSort scoresSort scoresNil

  -- cross axiom5
  crossAxiom5 cr1 scoresConsFd jtConsFd joinCtor studentSort studentsSort scoreSort scoresSort

  solverPush

  -- actual table
  studentsTableSym <- mkStringSymbol "Students"
  studentsTable <- mkConst studentsTableSym studentsSort
  studentRow1Sym <- mkStringSymbol "Student1"
  studentRow2Sym <- mkStringSymbol "Student2"
  studentRow1 <- mkConst studentRow1Sym studentSort
  studentRow2 <- mkConst studentRow2Sym studentSort

  scoresTableSym <- mkStringSymbol "Scores"
  scoresTable <- mkConst scoresTableSym scoresSort
  scoreRow1Sym <- mkStringSymbol "Score1"
  scoreRow2Sym <- mkStringSymbol "Score2"
  scoreRow1 <- mkConst scoreRow1Sym scoreSort
  scoreRow2 <- mkConst scoreRow2Sym scoreSort

  assert =<< mkEq studentsTable =<<
    (mkApp studentsConsFd [studentRow2, studentsNil] >>=
      (\r1 -> mkApp studentsConsFd [studentRow1, r1]))
  assert =<< mkEq scoresTable =<<
    (mkApp scoresConsFd [scoreRow2, scoresNil] >>=
      (\r1 -> mkApp scoresConsFd [scoreRow1, r1]))

  studentPK1 <- mkApp nrProj [studentRow1]
  studentPK2 <- mkApp nrProj [studentRow2]
  assert =<< mkDistinct [studentPK1, studentPK2]

  assert =<< mkNot =<< mkEq jtNil =<<
    (mkApp cross1 [studentsTable, scoresTable] >>=
      (\t1 -> mkApp filter1 [t1] >>=
        (\t2 -> mkApp filter2 [t2])))

  (_res, mbModel) <- getModel
  case mbModel of
    Just model -> showModel model
    Nothing    -> return "Couldn't construct model"
