-- hw1_3 part a
applyAll :: [b -> c] -> b -> [c]
applyAll funcs val = map (\func -> func val) funcs

-- hw1_3 part b
type Property a = [a -> Bool]
satisfies :: Property a -> a -> Bool
satisfies prop val = and $ applyAll prop val

-- hw1_3 part c
power :: ( a -> a) -> Int -> a -> a
power _ 0 x = x
power func num x = power func (num - 1) $ func x

-- hw1_3 part d
plus :: Int -> Int -> Int
plus a b
  | (a > 0) && (b > 0) = power succ a b
  | (a < 0) && (b > 0) = power pred (abs a) b
  | otherwise = power pred (abs b) a


times :: Int -> Int -> Int
times x y
  |( x < 0) == (y < 0) = power (plus (abs x)) (abs y) 0
  | otherwise = negate (power (plus (abs x)) (abs y) 0)

main :: IO ()
main = do
  print "part a: applyAll [(+1) , (*2), (/3)] 5"
  print (show (applyAll [(+1) , (*2), (/3)] 5))
  print "part b: satisfies [ even , True, (\\x -> (>3) )] 4"
  print (show (satisfies [ even , const True, (>3) ] 4))
  print "part b: satisfies [ even , True, (\\x -> (>3) )] 5"
  print (show (satisfies [ even , const True, (>3) ] 5))
  print "part c: power (2:) 5 []"
  print (show (power (2:) 5 []))
  print "part d: plus 1 1"
  print (show (plus 1 1))
  print "part d: plus 1 (-2)"
  print (show (plus 1 (-2)))
  print "part d: plus (-1) (-1)"
  print (show (plus (-1) (-1)))
  print "part d: times 5 1"
  print (show (times 5 1))
  print "part d: times (-1) 5"
  print (show (times (-1) 5))
  print "part d: times 5 (-1)"
  print (show (times 5 (-1)))
  print "part d: times 1 (-5)"
  print (show (times 1 (-5)))
  print "part d: times 0 (-5)"
  print (show (times 0 (-5)))
