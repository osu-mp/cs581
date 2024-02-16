module Hw3_group2 where

-- 1 Imperative language
type Name = String
type Val = Int
data Fun = Succ | Add Name
data Stmt = Assign Name Int | Apply Fun Name | Twice Stmt
type Prog = [Stmt]
type State = [(Name, Val)]

semStmt :: Stmt -> State -> State
semStmt (Assign n x) st      = (n, x):st
semStmt (Twice s)    st      = semStmt s (semStmt s st)
semStmt (Apply Succ n)    st = (n, (getVal n st) + 1):(removeVar n st)
semStmt (Apply (Add v) n) st = (n, ((getVal v st) + (getVal n st))):(removeVar n st)

getVal:: Name -> State -> Val
getVal n s = head ([i | (w,i) <- s, n==w] ++ [0])

removeVar :: Name -> State -> State
removeVar n s = ([(w, i) | (w,i) <- s, n/=w])

semProg :: Prog -> State
semProg [] = []
semProg xs = semP xs []

semP:: Prog -> State -> State
semP []    s = s
semP(x:xs) s = semP xs (semStmt x s)

-- 2 Mini Logo

data Cmd
  = Pen Mode
  | MoveTo Int Int
  | Sequ Cmd Cmd
  deriving (Show)

data Mode = Up | Down deriving (Show)

type MiniState = (Mode, Int, Int)

type Line = (Int, Int, Int, Int)

type Lines = [Line]

semL :: Cmd -> MiniState -> (MiniState, Lines)
semL (Pen m) (_, x, y) = ((m, x, y), [])
semL (MoveTo a b) (Down, x, y) = ((Down, a, b), [(x, y, a, b)])
semL (MoveTo a b) (Up, x, y) = ((Up, x, y), [])
semL (Sequ c1 c2) s0 =
  let (s1, lines1) = semL c1 s0
      (s2, lines2) = semL c2 s1
   in (s2, lines1 ++ lines2)

run :: Cmd -> Lines
run c = snd (semL c (Up, 0, 0))

sampleProg =
  Pen Down
    `Sequ` MoveTo 1 1
    `Sequ` MoveTo 0 1
    `Sequ` MoveTo 0 0
    `Sequ` Pen Up
    `Sequ` MoveTo 50 50


-- 3 Stack language

-- abstract syntax and supported operations
type S = [Op]
data Op = LD Int | ADD | SWAP | DUP

-- stack is a list of integers
type Stack = [Int]

-- result of operation can be either a stack or a failure string
data Result a = Success a | Fail String
semOp :: Op -> Stack -> Result Stack

-- load given int to top of stack
semOp (LD x) stack = Success (x : stack)

-- add the first 2 elements on the stack or error if fewer than 2 elements present
semOp ADD (x:y:stack) = Success (x + y : stack)
semOp ADD _ = Fail ("Error: ADD requires at least 2 elements on stack") 

-- swap first 2 elements or produce error if fewer than 2 elements
semOp SWAP (x:y:stack) = Success (y : x: stack)
semOp SWAP _ = Fail ("Error: SWAP requires at least 2 elements on stack")

-- duplicate top element and push onto stack
-- if stack is empty, produce error
semOp DUP [] = Fail ("Error: DUP requires at least 1 element on stack")
semOp DUP (x:xs) = Success (x:x:xs)

semS :: Stack -> [Op] -> Result Stack
semS stack [] = Success stack
semS stack (op:ops) = 
    case semOp op stack of
        Success newStack -> semS newStack ops
        Fail err         -> Fail err