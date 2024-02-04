-- Mini Logo

-- Mini Logo is a simplified version of the Logo language for programming
-- 2D graphics. The idea behind Logo and Mini Logo is to describe simple
-- line graphics through commands to move a pen from one position to
-- another. The pen can either be "up" or "down". Positions are given
-- by pairs of integers. Functions can be defined (using def) and called
-- (using call) to reuse groups of commands. The syntax of Mini Logo is as
-- follows

-- part a: define the abstract syntax for mini logo as Cmd

data Mode = Up | Down deriving (Show)

data Coord = Ints (Int, Int) | Names (String, String) deriving (Show)
type Pos = Int

data Pars = Pars String Pars | Name String deriving (Show)

data Vals = Vals Int Vals | Int deriving (Show)

data Cmd
  = Pen Mode
  | MoveTo Coord
  | Def String Pars Cmd
  | Call String Vals
  | Seq Cmd Cmd
  deriving (Show)

seqCmds :: [Cmd] -> Cmd
seqCmds [cmd] = cmd
seqCmds (cmd : cmds) = Seq cmd (seqCmds cmds)

-- part b: Write Mini Logo function `vector`
-- def vector (x1, y1, x2, y2) pen up ; moveto (x1, y1);
-- pen down; moveto (x2, y2); pen up

vector :: Cmd
vector =
  Def
    "vector"
    (Pars "x1" (Pars "y1" (Pars "x2" (Name "y2"))))
    ( seqCmds
        [ Pen Up,
          MoveTo (Names ("x1", "y1")),
          Pen Down,
          MoveTo (Names ("x2", "y2")),
          Pen Up
        ]
    )

-- part c: define a haskell function steps :: Int -> Cmd
-- that constructs a Mini Logo program to draw a stair
-- on n steps.

moveTo x y = MoveTo (Ints (x, y))
--moveTo :: Pos -> Pos  -> Cmd
--moveTo x y = MoveTo x y

step :: Int -> Int -> Cmd
step 1 1 =
      seqCmds
        [ Pen Up,
          moveTo 1 1,
          Pen Down,
          moveTo 0 1,
          moveTo 0 0
        ]
step x y =
      seqCmds
        [ Pen Up,
          moveTo x y,
          Pen Down,
          moveTo (x - 1) y,
          moveTo (x - 1) (y - 1),
          step (x - 1) (y - 1)
        ]

steps :: Int -> Cmd
steps n = step n n

main :: IO ()
main = do
  print "part b: vector function"
  print vector
  print "part c: steps 1"
  print (steps 1)
  print "part c: steps 4"
  print (steps 4)
