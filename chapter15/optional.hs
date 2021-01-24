module Optional where
import Data.Monoid
data Optional a = 
    Nada
    | Only a
    deriving (Eq,Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
-- now semigroup implements mappend in a from of <>
instance Semigroup a=> Semigroup (Optional a) where
   Nada <> Nada = Nada
   Nada <> (Only a) = Only a
   (Only a) <> Nada = Only a
   (Only a) <> (Only b) = Only (a<>b)
