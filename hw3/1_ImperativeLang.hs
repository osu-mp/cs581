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

main :: IO ()
main = do 
    print("Expect (x, 1) for [Apply Succ 'x']")
    print(semProg [Apply Succ "x"])
    print("Expect (x, 2) for [Twice (Apply Succ 'x')]")
    print(semProg [Twice (Apply Succ "x")])
    print("Expect (x, 10) for [Assign 'x' 5,Apply (Add 'x') 'x']")
    print(semProg [Assign "x" 5, Apply (Add "x") "x"])