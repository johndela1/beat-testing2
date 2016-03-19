import Text.Printf


--solveRPN :: (Num a) => String -> a
--solveRPN expression = head (foldl foldingFunction [] (words expression))
 --   	where   foldingFunction stack item = ...


calc :: (Num a) => String -> a
calc = head . (foldl f []) . words
	where f stack item = [-4]

res = calc "10 4 3 + 2 * -"

main = do
	printf "res: %s\n" (show res)
	printf "pass: %s\n" (show (res == (fromIntegral (-4))))


