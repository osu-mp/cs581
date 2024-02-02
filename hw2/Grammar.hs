-- Grammar.hs
module Grammar where

type NT = String
type Term = String

data Symbol = NT String | Term String
  deriving Show

data RHS = RHSEmpty | RHSNonEmpty Symbol [Symbol]
  deriving Show

data Prod = Production NT [RHS]
  deriving Show

data Grammar = Grammar [Prod]
  deriving Show