module ListSeq where
import Seq
import Par
import qualified Arr as A
import Arr ((!))

{--   
  scanS      :: (a -> a -> a) -> a -> s a -> (s a, a)
  Contract and expand defined at top cause its used in reduce and scan
--}

contract :: (a -> a -> a) -> (A.Arr a) -> (A.Arr a)
contract f xs = let half = (lengthS xs + 1) `div` 2  
                in tabulateS g $ half
    where n    = lengthS xs
          half = (n + 1) `div` 2
          p i  = i < half - 1 || even n
          g i  = if p i then f (xs ! (2*i)) (xs ! (2*i+1))
                 else xs ! (2*i)

expand :: (a -> a -> a) -> (A.Arr a) -> (A.Arr a) ->  (A.Arr a)
expand f xs s' = case lengthS xs of 
        0 -> s'
        1 -> s'
        n -> tabulateS g n
    where g i | even i    = s' ! (i `div` 2)
              | otherwise = f (s' ! (i `div` 2)) (xs ! (i-1))

instance Seq A.Arr where
    emptyS       = A.empty
    fromList     = A.fromList
    singletonS x = fromList [x]
    lengthS      = A.length
    nthS         = (!)
    tabulateS    = A.tabulate
    joinS        = A.flatten
    mapS f xs = case lengthS xs of 
            0 -> emptyS
            n -> tabulateS f' n 
            where f' i = f $ xs ! i
    filterS p xs  = case lengthS xs of
            0 -> emptyS
            n -> let s = tabulateS pByIndex n
                 in joinS s
            where pByIndex i = if p (xs ! i) then singletonS (xs ! i)
                               else emptyS
    appendS xs ys = joinS $ tabulateS f $ ns + nt
            where ns = lengthS xs
                  nt = lengthS ys
                  f i = if i < ns then singletonS $ xs ! i
                     else singletonS $ ys ! (i - ns)
    takeS xs n = A.subArray 0 n xs
    dropS xs n = A.subArray n (lengthS xs - n) xs   
    showtS xs  = case lengthS xs of
            0 -> EMPTY
            1 -> ELT $ xs ! 0
            n -> let sz = n `div` 2
                     (l,r) = takeS xs sz ||| dropS xs sz
                 in NODE l r
    showlS xs = case lengthS xs of 
            0 -> NIL
            n -> CONS (xs ! 0) (dropS xs 1)
    reduceS f e xs = case lengthS xs of
            0 -> e
            1 -> f e $ xs ! 0
            _ -> reduceS f e $ contract f xs
    scanS f e xs = case lengthS xs of
            0 -> (emptyS, e)
            1 -> (singletonS e, f e $ xs ! 0)
            _ -> let s' = scanS f e $ contract f xs
                     r  = expand f xs $ fst s'
                 in (r, snd s')
