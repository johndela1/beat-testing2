nums = concat [[n | m <- [1..2]] | n <- [1..6]]
--nums = concat [take 3 (repeat x) | x <- [1..10]]

merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

shuffle xs n
        | n > 0  = shuffle (merge (reverse a) b) (n-1)
        | otherwise = xs
    where (a,b) = splitAt (quot (length xs) 2) xs

anneal world n
    | n > 0 = anneal world (n-1)
    | otherwise = world

main = do
    let world = zip [1..] (map (\x->[x]) (shuffle nums (quot (length nums) 2)))
    print  world
    print  $ (anneal world 1)
