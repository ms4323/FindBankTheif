{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits
import Data.List

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret fp1 fp2 = do
    f1 <- BS.readFile fp1
    f2 <- BS.readFile fp2
    return $ BS.pack ( filter (/=0) ( zipWith xor (BS.unpack f1) (BS.unpack f2)) )

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key fp = do
   f <- BS.readFile fp
   BS.writeFile "victims.json" (BS.pack (zipWith xor (BS.unpack key) (BS.unpack f)))

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile fp = do
   f <- BS.readFile fp
   return $ decode f

-- Exercise 4 -----------------------------------------


getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs fp1 fp2 = do
  victims  <- parseFile fp1
  trans <- parseFile fp2

  let removem :: Maybe [a] -> [a]
      removem (Just a) = a
      removem  Nothing = []
  return $ Just(exists (removem victims) (removem trans))
             where exists :: [TId] -> [Transaction] -> [Transaction]
                   exists v t = filter (\a-> tid a `elem` v ) t


-- Exercise 5 -----------------------------------------


getFlow :: [Transaction] -> Map String Integer
getFlow [] = Map.empty
getFlow [t] = Map.fromList [(from t,-amount t), (to t, amount t) ]
getFlow (t:ts) = Map.fromList [(from t,-amount t), (to t, amount t) , head (Map.toList (getFlow ts)) ]

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal ma = fst ( maximum' (Map.toList ma))
                 where maximum' :: Ord v => [(k, v)] -> (k, v)
                       maximum' []     = error "maximum of empty list"
                       maximum' (x:xs) = maxTail x xs
                                 where maxTail currentMax [] = currentMax
                                       maxTail (m, n) (y:ys)
                                         | n < snd y = maxTail y ys
                                         | otherwise   = maxTail (m, n) ys

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m tids = payingBack payer payee tids
                   where (payer, payee) = partition ((>0) . snd) ( sortBy comp ( Map.toList m) )
                         comp :: (Ord v) => (k,v) -> (k,v) -> Ordering
                         comp (_,v1) (_,v2) = compare v1 v2

                         payingBack :: [(String,Integer)] -> [(String,Integer)] -> [TId] -> [Transaction]
                         payingBack (x:xs) (y:ys) (t:ts)
                                         | snd x == 0 = payingBack xs (y:ys) (t:ts)
                                         | snd y == 0 = payingBack (x:xs) ys (t:ts)
                                         | snd x == snd y = newTrans : payingBack xs ys ts
                                         | otherwise = payingBack ((fst x,snd x - am x y ):xs) ((fst y, snd y + am x y ):ys)  ts
                                         where newTrans = Transaction {from = fst x, to = fst y, amount = am x y, tid = t}
                                               am :: (String, Integer) -> (String , Integer) -> Integer
                                               am a b = min (abs (snd a)) (abs ( snd b))

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON fp a = BS.writeFile fp (encode a)

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
