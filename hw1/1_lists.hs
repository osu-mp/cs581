-- testing file for problem 1 functions (bag/list operations)

import Hw1_group2   -- use the functions defined in hw1_group2.hs

main :: IO ()
main = do
    let emptyBag = [] :: Bag Int  -- empty bag
    print("Empty ", emptyBag)

    putStrLn "\n(a) - Checks for insert"
    -- add a num into the empty bag
    let bag1 = ins 9 emptyBag  
    print("bag1 ", bag1)

    -- insert a different number twice, original again
    let bag2 = (ins 9 (ins 5 (ins 5 bag1)))
    print("bag2 ", bag2)

    putStrLn "\n(b) Checks for delete"
    let bag3 = del 5 bag2
    print("bag3 ", bag3)
    
    putStrLn "\n(c) - Checks for init"
    let xs = [7,3,8,7,3,2,7,5]
    let x_bag = bag xs
    print("x_bag ", x_bag)

    putStrLn "\n(d) - Checks for subbag"
    let x_big_list = [7,3,8,7,3,2,7,5,6,6,6]    -- contains extra 6s not in x_bag
    let x_big_bag = bag x_big_list
    let result = subbag x_bag x_big_bag
    print("subbag x_bag x_big_bag: ", result)   -- expect True
    let result = subbag x_big_bag x_bag
    print("subbag x_big_bag x_bag: ", result)   -- expect False

    putStrLn "\n(e) Checks for isSet"
    print("isSet emptyBag: ",  isSet( emptyBag ) )  -- expect True
    print("isSet bagWithOne: ",  isSet( bag1 ) )    -- expect True
    print("isSet x_bag: ", isSet( x_bag ) )         -- expect False

    putStrLn "\n(f) Checks for size"
    print("size emptyBag: ",  size( emptyBag ) )
    print("size bagWithOne: ",  size( bag1 ) )
    print("size x_bag: ", size( x_bag ) )             -- expect 8
    print("size x_big_bag: ", size( x_big_bag ) )   -- expect 11

    putStrLn "\nDONE" 