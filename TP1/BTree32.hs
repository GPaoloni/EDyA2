module BTree32 where

data BTree32 k a = Nil               --  ́arbol vacío
                 | Node
                   (BTree32 k a)     -- sub ́arbol izquierdo
                   Int               -- tamaño del árbol
                   (k,a)             -- elemento del nodo
                   (BTree32 k a)     -- sub ́arbol derecho
                   deriving Show

size :: BTree32 k a -> Int
size Nil            = 0
size (Node _ n _ _) = n

search :: Ord k => k -> BTree32 k a -> Maybe a
search _ Nil                 = Nothing
search k (Node l _ (c,x) r) | k == c = Just x
                             | k <  c = search k l
                             | otherwise  = search k r

-- Contruye un BTree32 Node con la estructura solicitada
node :: BTree32 k a -> (k,a) -> BTree32 k a -> BTree32 k a
node l x r = Node l sz x r
            where sz = size l + 1 + size r

singleR :: Ord k => BTree32 k a -> (k,a) -> BTree32 k a -> BTree32 k a
singleR (Node lb _ b rb) a ra = node lb b (node rb a ra)

doubleR :: Ord k => BTree32 k a -> (k,a) -> BTree32 k a -> BTree32 k a
doubleR (Node lb _ b (Node lc _ c rc)) a ra = node (node lb b lc) c (node rc a ra)

singleL :: Ord k => BTree32 k a -> (k,a) -> BTree32 k a -> BTree32 k a
singleL la a (Node lb _ b rb) = node (node la a lb) b rb

doubleL :: Ord k => BTree32 k a -> (k,a) -> BTree32 k a -> BTree32 k a
doubleL la a (Node (Node lc _ c rc) _ b rb) = node (node la a lc) c (node rc b rb)

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
