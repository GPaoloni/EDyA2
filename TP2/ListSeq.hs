
module ListSeq where
import Seq
import Par



instance Seq [] where
	emptyS         			= []
	singletonS x   			= [x]
	lengthS 	 			= length
	nthS		    	    = (!!)
	tabulateS f 0 		    = []
	tabulateS f n  			= let (h,t) = f 0 ||| tabulateS (f . (+1)) (n-1) 
							in h:t
	mapS f []				= []
	mapS f (x:xs)			= let (h,t) = f x ||| map f xs 
							in h:t
	filterS p []			= []
	filterS p (x:xs)	 	= let (truthy,t) = p x ||| filterS p xs
							in if truthy then x:t 
							   else t
	appendS 				= (++)
	takeS 					= take
	dropS 					= drop
	showtS []				= EMPTY
	showtS [x]				= ELT x
	showtS xs				= let (l,r) = take n xs ||| drop n xs
							  in NODE l r
					where n = lengthS xs `div` 2
	showlS []				= NIL 
	showlS (x:xs)			= CONS x $ showlS xs
	joinS					= concat TODO: CONSULTAR CON CECI
	reduceS f e []			= e
	reduceS f e [x]			= x
	reduceS f e (x:y:xs)	= let (h,t) = f x y ||| reduceS f e xs
							  in f h t
	scanS f e []			= ([], e)
	scanS f e [x]	 		= ([e], f e x)
	scanS f e  s	        = let s' = scanS f e $ contract s
							  in expand s s'
					where contract []       = []
						  contract [x]      = [x]
						  contract (x:y:xs) = let (h,t) = f x y ||| contract xs
						                      in h:t
						  expand xs (ys, r)	= (mergeAndReduce xs ys, r)
						  mergeAndReduce 
						   a -> (s a, a)
	fromList   :: [a] -> s a

	
	
	
	
