module Hw3_group2 where

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