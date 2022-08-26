-- Ingrid Lima dos Santos

-- q1. Escreva  uma  função  para  o  cálculo  dos  números  da sequência  de  Fibonacci,  utilizando  Haskell.
fibonacci :: Int -> Int
fibonacci n = fib !! n
  where
    fib = 0 : 1 : zipWith (+) fib (tail fib)

-- q2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor Comum  (MDC) de Euclides publicado por volta do  ano 300 AC. Podemos simplificar este algoritmo dizendo que dados dois inteiros A e B, o MDC entre eles será dado pelo valor absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva uma função para o cálculo do MDC entre dois números inteiros positivos,  usando o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell.
mdc :: Int -> Int -> Int
mdc a b
  | a < b = mdc b a
  | b == 0 = a
  | otherwise = mdc b (mod a b)

-- q3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste número. Exemplo: dado 1234 a função deverá devolver 10. Utilizando Haskell e recursividade.
somaDigs :: Int -> Int
somaDigs 0 = 0
somaDigs n = (n `mod` 10) + somaDigs (n `div` 10)

-- q5. Escreva uma função que, recebendo uma lista de inteiros, apresente  a diferença entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade.
somaQuads :: [Int] -> Int
somaQuads [] = 0
somaQuads (x : xs) = x ^ 2 + somaQuads xs

quadsSoma :: [Int] -> Int
quadsSoma [] = 0
quadsSoma (x : xs) = x + quadsSoma xs

diferenca :: [Int] -> Int
diferenca xs = (somaQuads xs) - (quadsSoma xs) ^ 2

-- q6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores que um determinado inteiro dado.
euler :: Int -> [Int]
euler n = euler' [2 .. n]
  where
    euler' [] = []
    euler' (x : xs) = x : euler' (filter (\y -> y `mod` x /= 0) xs)

-- q7. Nem só de Fibonacci vivem os exemplos de recursão. Escreva uma função que devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado.
lucas :: Int -> [Int]
lucas 0 = [2]
lucas 1 = [1, 2]
lucas n = (head (lucas (n - 1)) + head (lucas (n - 2))) : lucas (n - 1)

-- q8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] devolva [3,2,1].
aoContrario :: [Int] -> [Int]
aoContrario [] = []
aoContrario (x : xs) = aoContrario xs ++ [x]

-- q9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto destes valores sem usar o operador de multiplicação.
somaRecursiva :: Int -> Int -> Int
somaRecursiva a 1 = a
somaRecursiva a b = a + somaRecursiva a (b - 1)

-- q10. Escreva uma função chamada comprimento que receba uma lista de  inteiros e devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule o comprimento de uma lista.
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (x : xs) = 1 + comprimento xs

main :: IO ()
main = do
  putStrLn $ "\nFunc. 1: entrada: 11; resultado: " ++ show (fibonacci 11)
  putStrLn $ "\nFunc. 2: entrada: 2 4; resultado: " ++ show (mdc 2 4)
  putStrLn $ "\nFunc. 3: entrada: 1234; resultado: " ++ show (somaDigs 1234)
  -- q4. Escreva uma função que devolva a soma de todos os números menores que 10000 que sejam múltiplos de 3 ou 5.
  putStrLn $ "\nFunc. 4: entrada: 10000; resultado: " ++ show (sum ([n | n <- [1 .. 9999], n `mod` 3 == 0 || n `mod` 5 == 0]))
  putStrLn $ "\nFunc. 5: entrada: [1,2,3,4,5]; resultado: " ++ show (diferenca [1, 2, 3, 4, 5])
  putStrLn $ "\nFunc. 6: entrada: 30; resultado: " ++ show (euler 30)
  putStrLn $ "\nFunc. 7: entrada: 12; resultado: " ++ show (reverse (lucas 12))
  putStrLn $ "\nFunc. 8: entrada: [1,2,3,4,5,6,7,8,9]; resultado: " ++ show (aoContrario [1, 2, 3, 4, 5, 6, 7, 8, 9])
  putStrLn $ "\nFunc. 9: entrada: 8 8; resultado: " ++ show (somaRecursiva 8 8)
  putStrLn $ "\nFunc. 10: entrada: [1, 2, 3, 4, 5, 6, 7, 10, 20, 15]; resultado: " ++ show (comprimento [1, 2, 3, 4, 5, 6, 7, 10, 20, 15])
