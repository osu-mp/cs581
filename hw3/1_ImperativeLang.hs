import Hw3_group2

main :: IO ()
main = do 
    print("Expect (x, 1) for [Apply Succ 'x']")
    print(semProg [Apply Succ "x"])
    print("Expect (x, 2) for [Twice (Apply Succ 'x')]")
    print(semProg [Twice (Apply Succ "x")])
    print("Expect (x, 10) for [Assign 'x' 5,Apply (Add 'x') 'x']")
    print(semProg [Assign "x" 5, Apply (Add "x") "x"])