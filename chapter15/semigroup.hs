module Main where 
import Test.QuickCheck
import Data.Semigroup
------------------------------------1----------------------------------------
data Trivial  = Trivial deriving (Eq,Show)

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
------------------------------------2----------------------------------------
newtype Identity a = Identity a deriving (Eq,Show)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)
instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b) 

instance Arbitrary a=> Arbitrary (Identity a) where
    arbitrary = fmap Identity arbitrary

type IdentityAssoc a= Identity a->Identity a->Identity a->Bool

------------------------------------3----------------------------------------
data Two a b = Two a b deriving (Eq, Show)

instance (Monoid a, Monoid b)=>Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance (Semigroup a, Semigroup b)=>Semigroup (Two a b) where
    (Two a b) <> (Two a1 b1) = Two (a<>a1) (b<>b1)
instance (Arbitrary a, Arbitrary b)=> Arbitrary (Two a b) where
    arbitrary = do
        a<- arbitrary 
        b<- arbitrary 
        return (Two a b)

type TwoAssoc a b = Two a b->Two a b->Two a b->Bool

------------------------------------4----------------------------------------
data Three a b c= Three a b c deriving (Eq, Show)

instance (Monoid a, Monoid b, Monoid c)=>Monoid (Three a b c) where
    mempty = Three mempty mempty mempty 
    mappend = (<>)

instance (Semigroup a, Semigroup b, Semigroup c)=>Semigroup (Three a b c) where
    (Three a b c) <> (Three a1 b1 c1) = Three (a<>a1) (b<>b1) (c<>c1)

instance (Arbitrary a, Arbitrary b, Arbitrary c)=> Arbitrary (Three a b c) where
    arbitrary = do
        a<- arbitrary 
        b<- arbitrary 
        c<- arbitrary 
        return (Three a b c)

type ThreeAssoc a b c = Three a b c->Three a b c->Three a b c->Bool

------------------------------------5----------------------------------------
data Four a b c d= Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)=>Semigroup (Four a b c d) where
    (Four a b c d) <> (Four a1 b1 c1 d1) = Four (a<>a1) (b<>b1) (c<>c1) (d<>d1)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)=> Arbitrary (Four a b c d) where
    arbitrary = do
        a<- arbitrary 
        b<- arbitrary 
        c<- arbitrary 
        d<- arbitrary 
        return (Four a b c d)

type FourAssoc a b c d = Four a b c d->Four a b c d->Four a b c d->Bool


------------------------------------6----------------------------------------

newtype BoolConj = BoolConj Bool deriving (Eq,Show)

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend =  (<>)

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    _ <> _ = BoolConj False
instance Arbitrary BoolConj where
    arbitrary = fmap BoolConj arbitrary --frequency [(1, True), (1, False)]
type BoolConjAssoc = BoolConj->BoolConj->BoolConj->Bool 

------------------------------------7----------------------------------------

newtype BoolDisj = BoolDisj Bool deriving (Eq,Show)

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

instance Semigroup BoolDisj where
    _ <> (BoolDisj True) = BoolDisj True
    (BoolDisj True) <> _ = BoolDisj True
    _ <> _ = BoolDisj False
instance Arbitrary BoolDisj where
    arbitrary = fmap BoolDisj arbitrary --frequency [(1, True), (1, False)]
type BoolDisjAssoc = BoolDisj->BoolDisj->BoolDisj->Bool 

------------------------------------8----------------------------------------

data Or a b = 
    Fst a
    | Snd b
    deriving (Eq, Show)

instance Semigroup (Or a b) where
    _ <> (Snd b) = Snd b
    (Snd a) <> _ = Snd a
    (Fst a) <> (Fst b) = Fst b
instance (Arbitrary a, Arbitrary b)=>Arbitrary (Or a b) where
    arbitrary = oneof [fmap Fst arbitrary, fmap Snd arbitrary] 

type OrAssoc a b = Or a b->Or a b->Or a b->Bool 
-----------------------------------------------------------------------------

------------------------------------9----------------------------------------

newtype Combine a b = Combine { unCombine::a->b }

instance (Monoid b)=>Monoid (Combine a b) where
    mempty = Combine mempty
    mappend = (<>)

instance Semigroup b=>Semigroup (Combine a b) where
    (Combine a) <> (Combine b) = Combine (\x->a x <> b x)
------------------------------------10----------------------------------------

newtype Comp a = Comp { unComp::a->a }
instance Monoid a => Monoid (Comp a) where
    mempty = Comp id

instance Semigroup a=>Semigroup (Comp a) where
    (Comp a) <> (Comp b) = Comp (a.b)
------------------------------------11----------------------------------------
data Validation a b = Failure' a | Success' b
    deriving (Eq, Show)
instance (Semigroup a, Semigroup b)=>Semigroup (Validation a b) where
    (Failure' a) <> (Failure' b) =  Failure' (a<>b)
    (Success' a) <> (Success' b) =  Success' b
    Failure' a <> _ = Failure' a
    _ <> Failure' a = Failure' a
------------------------------------12----------------------------------------
newtype AccumulateRight a b = AccumulateRight (Validation a b)
    deriving (Eq, Show)
instance (Semigroup a, Semigroup b)=>Semigroup (AccumulateRight a b) where
    (AccumulateRight (Success' a)) <> (AccumulateRight (Success' b)) =
         AccumulateRight (Success' a<> Success' b) 
    (AccumulateRight (Failure' a)) <> _ = AccumulateRight (Failure' a)
    _ <> (AccumulateRight (Failure' a)) = AccumulateRight (Failure' a)
------------------------------------13----------------------------------------
newtype AccumulateBoth a b = AccumulateBoth (Validation a b)
    deriving (Eq, Show)
instance (Semigroup a, Semigroup b)=>Semigroup (AccumulateBoth a b) where
    (AccumulateBoth (Failure' a)) <> (AccumulateBoth (Failure' b)) =
         AccumulateBoth (Failure' a<> Failure' b)
    (AccumulateBoth (Success' a)) <> (AccumulateBoth (Success' b)) =
         AccumulateBoth (Success' a<> Success' b)
    (AccumulateBoth (Success' a)) <> (AccumulateBoth (Failure' b)) =
         AccumulateBoth (Failure' b)
    (AccumulateBoth (Failure' a)) <> (AccumulateBoth (Success' b)) =
         AccumulateBoth (Failure' a)
-----------------------------------------------------------------------------
newtype Mem s a = 
    Mem {
        runMem::s->(a,s)
    }
instance Semigroup a => Semigroup (Mem s a) where
    (Mem f) <> (Mem g) = Mem $ \s -> 
        let (a',s') = f s
            (a'',s'') = g s'
            in
                (a'' <> a',s'')
instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s->(mempty,s)
    mappend = (<>)
f' = Mem $ \s-> ("hi",s+1)
-----------------------------------------------------------------------------

semigroupAssoc::(Eq m, Semigroup m) => m->m->m->Bool 
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)



main::IO ()
main = do
    print $ runMem (f' <> mempty) 0 
    print $ runMem (mempty <> f') 0 
    print $ (runMem mempty 0::(String,Int)) 
    print $ runMem (f' <> mempty) 0 == runMem f' 0
    print $ runMem (mempty <> f') 0 == runMem f' 0
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (semigroupAssoc :: IdentityAssoc String)
    quickCheck (semigroupAssoc :: TwoAssoc String String)
    quickCheck (semigroupAssoc :: ThreeAssoc String String String)
    quickCheck (semigroupAssoc :: FourAssoc String String String String)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: OrAssoc String String)
    return ()