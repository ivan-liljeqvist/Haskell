--Ivan Liljeqvist and Filip Martinsson

module F1 where
import Data.Char 
import Data.List.Split

{-|
      fib returns the n'th fib number
      param 1: n - which fib number you want.
-}

fib :: Integer -> Integer

fib 0 = 0
fib 1 = 1
--recursively add, until we reach 0 or 1
fib n = fib(n-1)+fib(n-2)


{-|
      Takes a string and replaces each consonant 'x' with xox
      param 1: the string you want to encode
-}

rovarsprak :: String -> String
rovarsprak (x:xs) = if (x `elem` "jqwrtpsdfghklzxcvbnmJQWRTPSDFGHKLZXCVBNM")
    --if consonant - duplicate it and add o inbetween
    then x:'o':x:rovarsprak(xs)
    else x:rovarsprak(xs)
rovarsprak s = s

{-|
      Takes a string produced in rovarsprak and decodes it
      param 1: the string you want to decode
-}

karpsravor :: String -> String
karpsravor (x:xs) = if(x `elem` "jqwrtpsdfghklzxcvbnmJQWRTPSDFGHKLZXCVBNM")
    --if consonant - skip 2 following letters
    then x:karpsravor(tail(tail(xs)))
    else x:karpsravor(xs)

karpsravor s = s

{-|
      Takes a string and returns the average length of each word in it.
      param 1: the string you want to know average length of each word in
-}

medellangd :: String -> Double 
medellangd s = 
    fromIntegral (sum charactersInEachWordArray)/fromIntegral(length charactersInEachWordArray) --divide the total number of characters with the number of words
    where charactersInEachWordArray = map length(words (splitWords s)) --defined list. Each element in the list is the number of chars in corresponding word

{-|
      Takes a string and replaces each character that is not a letter with a space
      param 1: the string 
-}
splitWords :: String -> String
splitWords [] = []                         
splitWords (x:xs) =
   if(isAlpha x)then x: splitWords xs        
   else ' ': splitWords xs     


{-|
      Takes every second character and recursively adds the result of 'skyffla' of the rest of the array
-}

skyffla [] = []
skyffla list = every 2 list ++ skyffla(every 2 (tail list))

{-|
      Takes an array and return every n'th element in it.
      Param 1: n 
-}
every  :: Int -> [a] -> [a]
every n [] = []
every n s  = head s : every n (drop n s) 
 


              




