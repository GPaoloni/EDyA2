module ListSeq where
import Seq
import Par
import qualified Arr as A
import Arr ((!))

{--   
   reduceS    :: (a -> a -> a) -> a -> s a -> a
   scanS      :: (a -> a -> a) -> a -> s a -> (s a, a)
--}

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
            n -> let












