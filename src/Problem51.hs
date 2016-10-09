module Problem51 where

import Data.Numbers.Primes
import Data.Char
import Data.List
import Data.Ord
import Data.Function (on)

solutionPrimes = take 100000 (drop 10 primes)

stringPrimes :: [String]
stringPrimes = map show solutionPrimes

combinations k ns = filter ((k==).length) (subsequences ns)

substitueChar :: String -> Int -> Char -> String
substitueChar str i ch = (init (fst splices) ++ [ch]) ++ (snd splices)
                         where splices = splitAt i str

substitueDigits :: String -> [Int] -> Int -> String
substitueDigits num [] _ = num
substitueDigits num (x:xs) digit = substitueDigits (substitueChar num x (intToDigit digit)) xs digit

generateDigitSubstitutions :: String -> [Int] -> [String]
generateDigitSubstitutions num indexes = filter (\x -> not ((head x) == '0')) (map (substitueDigits num indexes) [0..9])

ignoreDivisables :: [String] -> [String]
ignoreDivisables nums = filter (\x -> not ((last x) `elem` ['0','2', '4', '5', '6', '8'])) nums

getPossibleIndexForSubstitution :: String -> [[Int]]
getPossibleIndexForSubstitution num = foldl (++) [] (map ((flip combinations) [1..(length num)]) [1..((length num) - 1)])

getNumSubstitutions :: String -> [[String]]
getNumSubstitutions num = map (generateDigitSubstitutions num) (getPossibleIndexForSubstitution num)

getNonDivisableSubstitutions :: String -> [[String]]
getNonDivisableSubstitutions num = map (ignoreDivisables) (getNumSubstitutions num)

ignoreNonPrimes :: [String] -> [String]
ignoreNonPrimes nums = filter (\x -> isPrime (read x)) nums

getPrimeSubstitutions :: String -> [[String]]
getPrimeSubstitutions num = map (ignoreNonPrimes) (getNonDivisableSubstitutions num)

getMaxNumberOfPrimeSubstitutions :: String -> [String]
getMaxNumberOfPrimeSubstitutions num = maximumBy (compare `on` length) (getPrimeSubstitutions num)

hasEnoughPrimeSubstitutions :: [String] -> Bool
hasEnoughPrimeSubstitutions num = (length num) >= 8

solution = find (hasEnoughPrimeSubstitutions) (map (getMaxNumberOfPrimeSubstitutions) stringPrimes)

