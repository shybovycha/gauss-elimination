split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split d s = [word] ++ split d xs'
	where
		word = takeWhile (/= d) s
		xs = drop (length word) s
		xs' = dropWhile (== d) xs