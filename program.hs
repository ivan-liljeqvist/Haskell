
module F1 where
import Data.Char 
import Data.List.Split


fib :: Integer -> Integer

fib 0 = 0
fib 1 = 1
fib n = fib(n-1)+fib(n-2)


rovarsprak :: String -> String
rovarsprak (x:xs) = if (x `elem` "jqwrtpsdfghklzxcvbnmJQWRTPSDFGHKLZXCVBNM")
    then x:'o':x:rovarsprak(xs)
    else x:rovarsprak(xs)
rovarsprak s = s


karpsravor :: String -> String
karpsravor (x:xs) = if(x `elem` "jqwrtpsdfghklzxcvbnmJQWRTPSDFGHKLZXCVBNM")
    then x:karpsravor(tail(tail(xs)))
    else x:karpsravor(xs)

karpsravor s = s


medellangd :: String -> Double 
medellangd s = 
    fromIntegral (sum charactersInEachWordArray)/fromIntegral(length charactersInEachWordArray) --divide the total number of characters with the number of words
    where charactersInEachWordArray = map length(words (splitWords s)) --defined list. Each element in the list is the number of chars in corresponding word

splitWords :: String -> String
splitWords [] = []                         
splitWords (x:xs) =
   if(isAlpha x)then x: splitWords xs        
   else ' ': splitWords xs     



skyffla [] = []
skyffla list = every 2 list ++ skyffla(every 2 (tail list))

every  :: Int -> [a] -> [a]
every n [] = []
every n s  = head s : every n (drop n s) 
 


              




