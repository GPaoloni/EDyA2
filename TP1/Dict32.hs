module Dict32 where
import BTree32

class Diccionario t where
    vacia :: Ord k => t k v
    insertar :: Ord k => (k,v) -> t k v -> t k v
    eliminar :: Ord k => k -> t k v -> t k v
    buscar :: Ord k => k -> t k v -> Maybe v

instance Diccionario BTree32 where
    vacia = Nil
    insertar k t = insert k t
    eliminar k t = delete k t
    buscar k t   = search k t
