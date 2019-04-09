data BTree32 k a = Nil               --  ́arbol vacío
                 | Node
                   (BTree32 k a)     -- sub ́arbol izquierdo
                   Int               -- tamaño del árbol
                   (k,a)             -- elemento del nodo
                   (BTree32 k a)     -- sub ́arbol derecho

size :: BTree32 k a -> Int
size Nil            = 0
size (Node _ n _ _) = n

search :: Ord k => k -> BTree k a -> Maybe a
search _ Nil                 = Nothing
search k (Node l _ (c,x) r) | k == c = Just x
                             | k <  c = search k l
                             | Other  = search k r

-- Contruye un BTree32 Node con la estructura solicitada
node :: BTree32 k a -> (k,a) -> BTree32 k a -> BTree32 k a
node l x r = Node l sz x r
            where sz = size l + 1 + size r

singleR :: Ord k => BTree32 k a -> (k,a) -> BTree32 k a -> BTree32 k a
singleR (Node (Node lb _ b rb) _ a ra) = node lb b (node rb a ra)

doubleR :: Ord k => BTree32 k a -> (k,a) -> BTree32 k a -> BTree32 k a
doubleR (Node (Node lb _ b (Node lc _ c rc)) _ a ra) = node (node lb b lc) c (node rc a ra)

singleL :: Ord k => BTree32 k a -> (k,a) -> BTree32 k a -> BTree32 k a
singleL (Node la _ a (Node lb _ b rb)) = node (node la a lb) b rb

doubleL :: Ord k => BTree32 k a -> (k,a) -> BTree32 k a -> BTree32 k a
doubleL (Node la _ a (Node (Node lc _ c rc) _ b rb) = node (node la a lc) c (node rc b rb) 
