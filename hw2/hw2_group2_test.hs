import Hw2_group2

main :: IO ()
main = do
  print "part b: vector function"
  print vector
  print "part c: steps 1"
  print (steps 1)
  print "part c: steps 4"
  print (steps 4)

  print "testing terminals"
  print (terminals imp)
  print "testing nonterminals"
  print (nonterminals imp)
