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
