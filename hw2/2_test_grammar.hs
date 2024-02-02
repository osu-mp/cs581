-- Test.hs
import Grammar

main :: IO ()
main = do
    -- symbols used in the right hand sides
    let myNT = "NonTerminal" :: NT
    let myTerm = "Terminal" :: Term

    -- right hand sides
    let emptyRHS = RHSEmpty
    let nonEmptyRHS = RHSNonEmpty (NT "SomeSymbol") [NT "AnotherSymbol"]

    -- productions for each non terminal?
    let prod1 = Production myNT [emptyRHS, nonEmptyRHS]
    let prod2 = Production myNT [emptyRHS]

    print prod1
    print prod2
    
    -- instance of grammar with the given productions
    let myGrammar = Grammar [prod1, prod2]

    -- debug print of grammar
    print myGrammar

    print "DONE"
