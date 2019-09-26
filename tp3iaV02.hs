signo :: Int -> Int
signo x | x > 0 = 1
	| x < 0 = -1
	| x == 0 = 0

entre0y9 :: Int -> Bool
entre0y9 x | (x < 0) = False
	   | (x > 9) = False
	   | (0 <= x) && (9 >= x) = True

rangoPrecio :: Int -> String
rangoPrecio x | (x < 0) = "Esto no puede ser!"
              | (x < 2000) = "Muy barato."
              | (x > 5000) = "Demasiado caro."
              | (x >= 2000) && (x <= 5000) = "Hay que verlo bien."

absoluto :: Int -> Int
absoluto x | (x >= 0) = x
           | (x < 0) = (-1) * x

esMultiplo2 :: Int -> Bool
esMultiplo2 x | (mod x 2) == 0 = True
              | not ((x `mod` 2) == 0) = False


esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | (mod y x) == 0 = True
                 | not ((mod x y) == 0) = False
                 | (y < x) = False

sumar1 :: [Int] -> [Int]
sumar1 [] = []
sumar1 (x:xs) = (x+1):(sumar1 xs)

solobinarios :: [Int] -> [Int]
solobinarios [] = []
solobinarios (x:xs) | (x==0 || x==1) = x:(solobinarios xs)
                    | (x/=0 && x/=1) = solobinarios xs

soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | (mod x 2) == 0 = x:(soloPares xs)
                 | (mod x 2) /= 0 = soloPares xs

mayoresQue10 :: [Int] -> [Int]
mayoresQue10 [] = []
mayoresQue10 (x:xs) | (x > 10) = x:(mayoresQue10 xs)
                    | (x < 10 || x == 10) = mayoresQue10 xs

mayoresQue :: Int -> [Int] -> [Int]
mayoresQue n [] = []
mayoresQue n (x:xs) | (x > n) = x:(mayoresQue n xs)
                    | (x < n || x == n) = mayoresQue n xs


duplica :: [Int] -> [Int]
duplica [] = []
duplica(x:xs) = (x*2):(duplica xs)

multiplica :: Int -> [Int] -> [Int]
multiplica n [] = []
multiplica n (x:xs) = (x*n):(multiplica n xs)

todosMenores10 :: [Int] -> Bool
todosMenores10[y] = y < 10
todosMenores10(x:y:xs) = (x < 10) && todosMenores10(y:xs)

