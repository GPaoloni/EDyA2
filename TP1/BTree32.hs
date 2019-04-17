{-
Implementación de Binary Search Tree 3-2
Trabajo práctico 1

Integrantes: 
Gianfranco Paoloni
Sebastián Zimmermann
-}

module BTree32 where

data BTree32 k a = Nil               -- árbol vacío
                 | Node
                   (BTree32 k a)     -- subárbol izquierdo
                   Int               -- tamaño del árbol
                   (k,a)             -- elemento del nodo
                   (BTree32 k a)     -- subárbol derecho
                   deriving Show

size :: BTree32 k a -> Int
size Nil            = 0
size (Node _ n _ _) = n

search :: Ord k => k -> BTree32 k a -> Maybe a
search _ Nil                = Nothing
search k (Node l _ (c,x) r) | k == c = Just x
                            | k <  c = search k l
                            | k >  c = search k r

-- Contruye un BTree32 Node con la estructura solicitada
node :: BTree32 k a -> (k,a) -> BTree32 k a -> BTree32 k a
node l x r = Node l sz x r
            where sz = size l + 1 + size r

singleR :: Ord k => BTree32 k a -> (k,a) -> BTree32 k a -> BTree32 k a
singleR (Node lb _ b rb) a ra = node lb b (node rb a ra)

doubleR :: Ord k => BTree32 k a -> (k,a) -> BTree32 k a -> BTree32 k a
doubleR (Node lb _ b (Node lc _ c rc)) a ra = node (node lb b lc) 
                                                   c
                                                   (node rc a ra)

singleL :: Ord k => BTree32 k a -> (k,a) -> BTree32 k a -> BTree32 k a
singleL la a (Node lb _ b rb) = node (node la a lb) b rb

doubleL :: Ord k => BTree32 k a -> (k,a) -> BTree32 k a -> BTree32 k a
doubleL la a (Node (Node lc _ c rc) _ b rb) = node (node la a lc) 
                                                   c
                                                   (node rc b rb)

balance :: Ord k => BTree32 k a -> (k , a) -> BTree32 k a -> BTree32 k a
balance l (k,v) r | size r + size l <= 1 = node l (k,v) r
                  | size r > 3 * size l  = reBalanceL l (k,v) r
                  | size l > 3 * size r  = reBalanceR l (k,v) r
                  | otherwise            = node l (k,v) r
                      
reBalanceL :: Ord k => BTree32 k a -> (k , a) -> BTree32 k a -> BTree32 k a
reBalanceL l (k,v) r | isSingle r = singleL l (k,v) r
                     | otherwise  = doubleL l (k,v) r
                     where isSingle (Node ll _ x rr) = size rr > 2 * size ll
                      
reBalanceR :: Ord k => BTree32 k a -> (k , a) -> BTree32 k a -> BTree32 k a
reBalanceR l (k,v) r | isSingle l = singleR l (k,v) r
                     | otherwise  = doubleR l (k,v) r
                     where isSingle (Node ll _ x rr) = size ll > 2 * size rr

insert :: Ord k => (k , a) -> BTree32 k a -> BTree32 k a
insert (j,x) Nil                = node Nil (j,x) Nil
insert (j,x) (Node l _ (k,v) r) | j > k      = balance l (k,v) (insert (j,x) r)
                                | j < k      = balance (insert (j,x) l) (k,v) r
                                | otherwise  = node l (k,x) r

delRoot :: Ord k => BTree32 k a -> BTree32 k a
delRoot (Node Nil _ (k,v) Nil) = Nil
delRoot (Node l   _ (k,v) r  ) | size l <  size r = node l             
                                                         (searchMin r)
                                                         (deleteMin r) 
                               | size l >= size r = node (deleteMax l) 
                                                         (searchMax l) 
                                                         r
                               

searchMin :: Ord k => BTree32 k a -> (k,a)
searchMin (Node Nil _ (k,v) _) = (k,v)
searchMin (Node l   _ _     _) = searchMin l

deleteMin :: Ord k => BTree32 k a -> BTree32 k a
deleteMin (Node Nil _ (k,v) Nil) = Nil
deleteMin (Node Nil _ (k,v) r  ) = r
deleteMin (Node l   _ (k,v) r  ) = node (deleteMin l) (k,v) r

searchMax :: Ord k => BTree32 k a -> (k,a)
searchMax (Node _ _ (k,v) Nil) = (k,v) 
searchMax (Node _ _ _     r  ) = searchMax r 

deleteMax :: Ord k => BTree32 k a -> BTree32 k a
deleteMax (Node Nil _ (k,v) Nil) = Nil
deleteMax (Node l   _ (k,v) Nil) = l
deleteMax (Node l   _ (k,v) r  ) = node l (k,v) (deleteMax r)

delete :: Ord k => k -> BTree32 k a -> BTree32 k a
delete x Nil                   = Nil 
delete x t@(Node l _ (k,v) r ) | x <  k = balance (delete x l) 
                                                  (k,v) 
                                                  r
                               | x >  k = balance l 
                                                  (k,v)
                                                  (delete x r)
                               | x == k = delRoot t

