module ListSeq where
import Seq
import Par


instance Seq [] where
    emptyS        = []
    singletonS x  = [x]
    lengthS       = length
    nthS          = (!!)
    tabulateS f 0 = []
    tabulateS f n = let (h,t) = f 0 ||| tabulateS (f . (+1)) (n-1)
                    in h:t
    mapS f []     = []
    mapS f (x:xs) = let (h,t) = f x ||| map f xs
                    in h:t
    filterS p []     = []
    filterS p (x:xs) = let (truthy,t) = p x ||| filterS p xs
                       in if truthy then x:t else t
    appendS    = (++)
    takeS n xs = take xs n
    dropS n xs = drop xs n
    showtS []  = EMPTY
    showtS [x] = ELT x
    showtS xs  = let (l,r) = take n xs ||| drop n xs
                 in NODE l r
        where n = lengthS xs `div` 2
    showlS []     = NIL
    showlS (x:xs) = CONS x xs
    joinS         = concat
    reduceS f e []       = e
    reduceS f e [x]      = x
    reduceS f e (x:y:xs) = let (h,t) = f x y ||| reduceS f e xs
                           in f h t
    scanS f e []  = ([], e)
    scanS f e [x] = ([e], f e x)
    scanS f e s   = let s' = scanS f e $ contract s
                    in expand s s'
        where
            contract []       = []
            contract [x]      = [x]
            contract (x:y:xs) = let (h,t) = f x y ||| contract xs
                                in h:t
            expand xs (ys, r)   = (completeScan xs ys, r)
            completeScan [x] ys = ys
            completeScan xs ys  = tabulateS (\i -> buildIthItem i xs ys) (lengthS xs)
            buildIthItem i xs ys = if even i
                                   then nthS ys $ i `div` 2
                                   else f (nthS ys $ i `div` 2) (nthS xs $ i-1)
    fromList = id
