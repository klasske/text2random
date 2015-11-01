import Data.Char (toLower) 
import Data.Set (toList, fromList)

--remove the . at the end of a sentence
removeDot :: [Char] -> [Char]
removeDot [] = []
removeDot xs 
  | last xs == '.' = init xs
  | otherwise = xs		

--prepare words for the Markov model
convert w = removeDot (map toLower w)

unique xs = toList $ fromList xs

getAllWords w pairs = [p2 | (p1, p2) <- pairs, p1 == w]
getCountPerWordMatrix w pairs = [p2 | (p1, p2) <- pairs, p1 == w]


countInList _ [] = 0 
countInList x xs
  | head xs == x = 1 + countInList x (tail xs)
  | otherwise = countInList x (tail xs)

-- for each item in a list, find how many 
getCountPerItem xs = [(x, countInList x xs) | x <- unique xs]

getCountPerWordPairs w pairs = getCountPerItem [p2 | (p1, p2) <- pairs, p1 == w]

main = do
  f <- readFile "example"
  -- make a list of all words in lowercase
  let w = map convert ( words [x | x <- f])
  --convert to pairs
  let pairs = [(x, y)| (x, y) <- zip w (tail w)]
  --print pairs
  -- get each word with the words that follow it in the text
  mapM_ print [(w', getCountPerWordPairs w' pairs)| w' <- unique w ]