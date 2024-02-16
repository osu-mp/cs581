-- Mini Logo

data Cmd
  = Pen Mode
  | MoveTo Int Int
  | Sequ Cmd Cmd
  deriving (Show)

data Mode = Up | Down deriving (Show)

type State = (Mode, Int, Int)

type Line = (Int, Int, Int, Int)

type Lines = [Line]

semL :: Cmd -> State -> (State, Lines)
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

-- imp end
-- test begin

main :: IO ()
main = do
  print "Tests for hw3 2_mini_logo"
  print "Running this sample mini logo program:"
  print sampleProg
  print "We output these lines:"
  print (run sampleProg)
