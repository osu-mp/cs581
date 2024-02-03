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

newtype Grammar = Grammar [Prod] deriving (Show)

data Prod = Prod NT [RHS] deriving (Show)

newtype RHS = RHS [Symbol] deriving (Show)

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

main :: IO ()
main = do
  print "testing terminals"
  print (terminals imp)
  print "testing nonterminals"
  print (nonterminals imp)
