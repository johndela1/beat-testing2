people = concat [[n | m <- [1..3]] | n <- [1..2]]
tables = [1..length people]
placements = (zip people tables) ++ [(100,n) | n <- [1..length tables]]
--placements = [(1,1),(1,2),(8,3),(9,4), (100,1),(100,2),(100,3),(100,4)]

merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

shuffle' xs n
        | n > 0  = shuffle' (merge (reverse a) b) (n-1)
        | otherwise = xs
    where (a,b) = splitAt (quot (length xs) 2) xs
shuffle xs = shuffle' xs (quot (length xs) 2)

anneal [] new_p = reverse new_p
anneal ((p,t):ps) new_p =
    anneal ps $ bestNeighbor:new_p
  where neighbors = filter (\(x,y)->y>=t-1&&y<=t+1) (ps++new_p)
        bestNeighbor = if p == 100 then (p,t) else best neighbors (p,t)

best' [] person acc = (fst person, snd acc)
best' ((p1,t1):ps) (p,t) acc
    | abs (p-p1) < abs (p-(fst acc)) = best' ps (p,t) (p1,t1)
    | otherwise = best' ps (p,t)  acc
best neighbors placement  = best' neighbors placement (head neighbors)

main = do
    -- print (best [(1,2),(9,4)] (2,3))
    print placements
    let foo = anneal placements []
    print foo
    print $ (anneal foo [])
remove i xs = filter (\n -> n/=i) xs
insert x (table, people) = (table, people++x)
random x = mod  (x * 1103515245 + 12345) 1000000000  
