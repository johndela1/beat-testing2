color n = "\x1b["++(show n)++"m"++" color "++show n
foo = map color [31..37]
main = do
    mapM_ putStrLn foo
