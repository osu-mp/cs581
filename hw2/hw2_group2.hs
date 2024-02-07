module Hw2_group2 where

-- Mini Logo

-- Mini Logo is a simplified version of the Logo language for programming
-- 2D graphics. The idea behind Logo and Mini Logo is to describe simple
-- line graphics through commands to move a pen from one position to
-- another. The pen can either be "up" or "down". Positions are given
-- by pairs of integers. Functions can be defined (using def) and called
-- (using call) to reuse groups of commands. The syntax of Mini Logo is as
-- follows

-- part a: define the abstract syntax for mini logo as Cmd

newtype Pars = Pars [String] deriving (Show)

newtype Vals = Vals [Int] deriving (Show)

data Mode
  = Up
  | Down
  deriving (Show)

data Coord
  = Ints (Int, Int)
  | Names (String, String)
  deriving (Show)

data Cmd
  = Pen Mode
  | MoveTo Coord
  | Def String Pars Cmd
  | Call String Vals
  | Seq [Cmd]
  deriving (Show)

-- part b: Write Mini Logo function `vector`
-- def vector (x1, y1, x2, y2) pen up ; moveto (x1, y1);
-- pen down; moveto (x2, y2); pen up

vector :: Cmd
vector =
  Def
    "vector"
    (Pars ["x1", "y1", "x2", "y2"])
    ( Seq
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

step :: Int -> Int -> [Cmd]
step x y =
  [ Pen Up,
    moveTo x y,
    Pen Down,
    moveTo (x - 1) y,
    moveTo (x - 1) (y - 1)
  ]

steps :: Int -> Cmd
steps n = Seq $ concat [step x x | x <- [1 .. (n + 1)]]


-- Grammar Grammar
-- Consider the following grammar that describes the syntax of the language
-- for grammar definitions

-- grammar ::= prop ; ... ; prod
-- prod ::= nt ::= rhs | ... | rhs
-- rhs ::= symbol*
-- symbol ::= nt | term

-- A grammar is given by a list of production groups (prod), each of which
-- consists of a non terminal nt and a list of alternative right-hand sides
-- (rhs). Each production group lists the productions for a particular
-- nonterminal. A right-hand side of a production is given by a sequence of
-- nonterminal (nt) and terminal (term) symbols.

type NT = String

type Term = String

data Symbol = NTSymbol NT | TermSymbol Term deriving (Show)

newtype RHS = RHS [Symbol] deriving (Show)

data Prod = Prod NT [RHS] deriving (Show)

newtype Grammar = Grammar [Prod] deriving (Show)



-- part b: imp

prodCond :: Prod
prodCond =
  Prod
    "cond"
    [ RHS [TermSymbol "T"],
      RHS [TermSymbol "not", NTSymbol "cond"],
      RHS [TermSymbol "(", NTSymbol "cond", TermSymbol ")"]
    ]

prodStmt :: Prod
prodStmt =
  Prod
    "stmt"
    [ RHS [TermSymbol "skip"],
      RHS
        [ TermSymbol "while",
          NTSymbol "cond",
          TermSymbol "do",
          TermSymbol "{",
          NTSymbol "stmt",
          TermSymbol "}"
        ],
      RHS
        [ NTSymbol "stmt",
          TermSymbol ";",
          NTSymbol "stmt"
        ]
    ]

imp :: Grammar
imp = Grammar [prodCond, prodStmt]

-- part c: extract terminals and nonterminals

ntInProd :: Prod -> NT
ntInProd (Prod nt _) = nt

tInRHS :: RHS -> [Term]
tInRHS (RHS symbols) = [term | TermSymbol term <- symbols]

tInProd :: Prod -> [Term]
tInProd (Prod _ rhs) = concat [tInRHS rh | rh <- rhs]

nonterminals :: Grammar -> [NT]
nonterminals (Grammar prods) = [ntInProd prod | prod <- prods]

terminals :: Grammar -> [Term]
terminals (Grammar prods) = concat [tInProd prod | prod <- prods]
