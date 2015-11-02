import Data.Char (toLower) 
import Data.List (sort, find)
import System.Random
import System.Environment (getArgs)
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
--convert w = removePre (foldl removeSuf (map toLower w) ".,)!") '('
convert w = removePre (foldl removeSuf w ".,)!") '('

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


-- find the word
-- get random word from its list (of length )
-- call function for next word with one less random number
--formStory :: String -> [(String, [String])] -> [Int] -> [String]
formStory _ _ [] = []
formStory w listOfWords randomNumbers 
  | s /= [] = (fst wordItem) ++ " " ++ (formStory (s !! (r `mod` (length s))) listOfWords (tail randomNumbers))
  | otherwise = fst wordItem
  where wordItem = head (findItem ((==) w . fst) listOfWords)
        s = snd wordItem
        r = head randomNumbers
   
defArgs [] = ("wuthr10.txt", "100", "the")
defArgs [filename] = (filename, "100", "the")
defArgs [filename, words] = (filename, words, "the")
defArgs [filename, words, first] = (filename, words, first)


main = do
  args <- getArgs
  let (fileName, numWords, first) = defArgs args

  f <- readFile fileName

  -- make a list of all words in lowercase
  let w = map convert ( words [x | x <- f])

  --convert to pairs
  let pairs = [(x, y)| (x, y) <- pair w]

  --print pairs
  -- get each word with the words that follow it in the text 
  let mapping = [(w', getCountPerWordPairs w' pairs)| w' <- unique w ]

  --now starting at a random word (let's say "a"), we could start printing 
  --the word with the highest likelihood. then find that 
  --word, and get the most likely word following that one
  let storyLength = read numWords

  --create a list of random numbers to generate the story
  n <- forM [1..storyLength] $ \_i -> (getStdRandom (randomR (0, (length mapping)-1)))
  print $ formStory first mapping n
