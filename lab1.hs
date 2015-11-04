import Data.Char
import Data.List
import Data.List.Split

buildRect :: Int -> [Char] -> [[Char]]
buildRect n [] = []
buildRect 0 (x:xs) = error "key length must not be 0"
buildRect n (xs) = let k = length(xs) 
					in if k >= n
						then [take n xs]  ++ buildRect n (drop n xs )
							else [ (take n xs )++ (replicate (n-k) '-' )]  ++ buildRect n (drop n xs )

encode :: [Int] -> [[Char]]  -> String
encode ns xs =  let cols = map (\n ->map (!! (n-1)) xs   ) ns
					in intercalate "" (map (\i -> cols !! (i-1)) ns) 
				
decode :: [Int] -> String -> String
decode k xs = let 
				lenk = length k
				ncols = (length xs) `div` lenk
				grouped = chunksOf ncols xs
				join = intercalate ""
				rect =  map (\i -> grouped !! (i-1)) k
					in join $ map (\j -> map ( \s -> s !! (j)) rect) [0..ncols-1] 

main = do
	let key = "5147263" --"1423756"
	--text <- readFile "text2.txt" -- "text.txt"
	let text = "VOTPRIMERSIFRAVERTIKALbNOYPERESTANOVKI"
	let keysnum = map digitToInt  key
	let rect = buildRect ( length key)  text 
	print $ rect
	putStr $ unlines $ rect
	print $ decode keysnum $ encode  keysnum rect
