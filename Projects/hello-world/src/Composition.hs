f x = x + 10

g x = x * x

main = print $ map (g . f) [1, 2, 3, 4, 5, 6]

