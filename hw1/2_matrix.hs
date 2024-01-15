type Row    = [Int]
type Column = [Int]
type Matrix = [Row]

-- 2.a Vector Addition


vAdd :: Row -> Row -> Row
vAdd [] [] = []
vAdd (x:xs) (y:ys) = x + y : vAdd xs ys


vAddZipWith :: Row -> Row -> Row
vAddZipWith x y = zipWith (+) x y

-- 2.b Matrix Addition
mAdd :: Matrix -> Matrix -> Matrix
mAdd [] [] = []
mAdd x y = zipWith vAdd x y

-- 2.c Inner Product
iProd :: Row -> Column -> Int
iProd [] [] = 0
iProd (x:xs) (y:ys) = (x * y) + iProd xs ys

-- 2.d Outer Product
oProd :: Column -> Row -> Matrix
oProd [] _ = []
oProd (c:cs) ys = map (\x -> c * x ) ys : oProd cs ys

-- 2.e Size

mSize :: Matrix -> (Int, Int)
mSize [] = (0,0)
mSize (x:xs) = let (r, c) = mSize xs in (r + 1, foldl (\acc x -> acc + 1) 0 x)





main :: IO ()
main = do
    putStrLn "\n(a) - Vector Addition"
    let v1 = [ 1, 2, 3, 4]
    let v2 = [ 2, 4, 6, 8]
    let vA = vAdd v1 v2
    let vAZ = vAddZipWith v1 v2

    print("Recursive Version: ", vA)
    print("zipWith Version:   ", vAZ)

    putStrLn "\n(b) - Matrix Addition"
    let m1 = [[1,2,3],
              [4,5,6],
              [7,8,9]]
    let m2 = [[10,20,30],
              [40,50,60],
              [70,80,90]]
    let mA = mAdd m1 m2
    print("Added together: ", mA)

    putStrLn "\n(c) - Inner Product"
    let vI = iProd v1 v2
    print("Inner product: ", vI)

    putStrLn "\n(d) - Outer Product"
    let vO = oProd v1 v2
    print("Outer product: ", vO)

    putStrLn "\n(e) - Size"
    let vO12 = oProd v1 v2
    let s12 = mSize vO12
    print("Size of outer product: ", s12)

    let v3 = [1,2,3]
    let v4 = [4,5,6,7]
    let vO34 = oProd v3 v4
    let s34 = mSize vO34
    let vO43 = oProd v4 v3
    let s43 = mSize vO43
    print("Outer product: ", vO34)
    print("Size of outer product: ", s34)
    print("Outer product: ", vO43)
    print("Size of outer product: ", s43)
    putStrLn "\nDONE"

