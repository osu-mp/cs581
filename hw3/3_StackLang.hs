import Hw3_group2

main :: IO ()
main = do
    
    putStrLn "\nTest - expect failure on second add"
    let initialStack = [] :: Stack
        operations = [LD 5, DUP, ADD, ADD]
        result = semS initialStack operations
    case result of
        Success stack -> putStrLn $ show stack
        Fail err      -> putStrLn $ err


    putStrLn "\nTest - Expect [6, 12] via  (3; 3,3; 6; 6,6; 6,6,6; 12,6; 6, 12)"
    let initialStack = [] :: Stack
        operations = [LD 3, DUP, ADD, DUP, DUP, ADD, SWAP]
        result = semS initialStack operations
    case result of
        Success stack -> putStrLn $ show stack
        Fail err      -> putStrLn $ err

    putStrLn "\nTest - Expect [] (no operations)"
    let initialStack = [] :: Stack
        operations = []
        result = semS initialStack operations
    case result of
        Success stack -> putStrLn $ show stack
        Fail err      -> putStrLn $ err

    putStrLn "\nTest - Expect [10,1,5] via (1; 5,1; 1,5; 1,1,5; 1,1,1,5; 2,1,5; 8,2,1,5; 10,1,5)"
    let initialStack = [] :: Stack
        operations = [LD 1, LD 5, SWAP, DUP, DUP, ADD, LD 8, ADD]
        result = semS initialStack operations
    case result of
        Success stack -> putStrLn $ show stack
        Fail err      -> putStrLn $ err

    putStrLn "\nTest - Expect failure when duplicating empty stack"
    let initialStack = [] :: Stack
        operations = [DUP]
        result = semS initialStack operations
    case result of
        Success stack -> putStrLn $ show stack
        Fail err      -> putStrLn $ err


    putStrLn "\nDONE"
    