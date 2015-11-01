import Data.Char (toLower) 
import Data.List (sort, find)
import System.Random
import Data.Traversable (forM)

--remove the . at the end of a sentence
removePre :: [Char] -> Char -> [Char]
removePre [] _ = []
removePre xs x
  | head xs == x = tail xs
  | otherwise = xs

--remove the . at the end of a sentence
removeSuf :: [Char] -> Char -> [Char]
removeSuf [] _ = []
removeSuf xs x
  | last xs == x = init xs
  | otherwise = xs
	

--prepare words
convert w = removePre (foldl removeSuf (map toLower w) ".,)!") '('

--make pairs out of a list
pair xs = zip xs (tail xs)

--find all unique items in a list
unique xs = [s1| (s1, s2) <- pair s, s1 /= s2] where s = sort xs


countInList _ [] = 0 
countInList x xs
  | head xs == x = 1 + countInList x (tail xs)
  | otherwise = countInList x (tail xs)

-- for each item in a list, find how many 
getCountPerItem xs = [(x, countInList x xs) | x <- unique xs]

--getCountPerWordPairs w pairs = getCountPerItem [p2 | (p1, p2) <- pairs, p1 == w]
getCountPerWordPairs w pairs = [p2 | (p1, p2) <- pairs, p1 == w]

findItem _ [] = []
findItem f xs
   | f (head xs) == True = [head xs]
   | otherwise = findItem f (tail xs)

--getNewWord item r 
--   | snd item == []

-- find the word
-- get random word from its list (of length )
-- call function for next word with one less random number
--formStory :: String -> [(String, [String])] -> [Int] -> [String]
formStory _ _ [] = []
formStory w listOfWords randomNumbers 
  | s /= [] = (fst wordItem) ++ " " ++ (formStory (s !! ((length s) `mod` (head randomNumbers) - 1)) listOfWords (tail randomNumbers))
  | otherwise = fst wordItem
  where wordItem = head (findItem ((==) w . fst) listOfWords)
        s = snd wordItem
   


main = do
  f <- readFile "example"
  -- make a list of all words in lowercase
  let w = map convert ( words [x | x <- f])
  --convert to pairs
  let pairs = [(x, y)| (x, y) <- pair w]
  --print pairs
  -- get each word with the words that follow it in the text 
  let mapping = [(w', getCountPerWordPairs w' pairs)| w' <- unique w ]
  --now starting at a random word (let's say "a"), we could start printing the word with the highest likelihood. then find that 
  --word, and get the most likely word following that one
  let storyLength = 50
  n <- forM [1..storyLength] $ \_i -> (getStdRandom (randomR (0, (length mapping) - 1)))
  -- mapM_ print (map ((!!) mapping) n)
  print $ formStory "an" mapping n