{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE Trustworthy          #-}
{-# LANGUAGE UndecidableInstances #-}

#define HAS_POLY_TYPEABLE MIN_VERSION_base(4,7,0)

#if HAS_POLY_TYPEABLE
{-# LANGUAGE StandaloneDeriving   #-}
#endif

-- | Fixed points of a functor.
module Data.Fix (
    -- * Fix
    Fix (..),
    unfix,
    hoistFix,
    hoistFix',
    foldFix,
    unfoldFix,
    -- * Mu - least fixed point
    Mu (..),
    hoistMu,
    foldMu,
    unfoldMu,
    -- * Nu - greatest fixed point
    Nu (..),
    hoistNu,
    foldNu,
    unfoldNu,
) where

import Data.Function        (on)
import Data.Functor.Classes
       (Eq1, Ord1, Read1, Show1, compare1, eq1, readsPrec1, showsPrec1)
import Data.Hashable        (Hashable (..))
import Data.Hashable.Lifted (Hashable1, hashWithSalt1)
import Data.Typeable        (Typeable)
import Text.Read
       (Lexeme (Ident), Read (..), lexP, parens, prec, readS_to_Prec, step)

#if MIN_VERSION_deepseq(1,4,3)
import Control.DeepSeq (NFData (..), NFData1, rnf1)
#endif

#if HAS_POLY_TYPEABLE
import Data.Data (Data)
#else
import Data.Data
#endif

-- $setup
-- >>> :set -XDeriveFunctor
-- >>> import Data.Functor.Classes
-- >>> data ListF a b = Nil | Cons a b deriving (Show, Functor)
--
-- >>> :{
-- >>> instance Show a => Show1 (ListF a) where
-- >>>     liftShowsPrec _  _ d Nil        = showString "Nil"
-- >>>     liftShowsPrec sp _ d (Cons a b) = showParen (d > 10) $ showString "Cons " . showsPrec 11 a . showChar ' ' . sp 11 b
-- >>> :}
--
-- >>> :{
-- >>> let elimListF n c Nil        = 0
-- >>>     elimListF n c (Cons a b) = c a b
-- >>> :}

-------------------------------------------------------------------------------
-- Fix
-------------------------------------------------------------------------------

newtype Fix f = Fix (f (Fix f))

unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f

hoistFix :: Functor f => (forall a. f a -> g a) -> Fix f -> Fix g
hoistFix nt = go where go (Fix f) = Fix (nt (fmap go f))

hoistFix' :: Functor g => (forall a. f a -> g a) -> Fix f -> Fix g
hoistFix' nt = go where go (Fix f) = Fix (fmap go (nt f))

-- | Fold 'Fix'.
--
-- >>> let fp = unfoldFix (\i -> if i < 4 then Cons i (i + 1) else Nil) (0 :: Int)
-- >>> foldFix (elimListF 0 (+)) fp
-- 6
--
foldFix :: Functor f => (f a -> a) -> Fix f -> a
foldFix f = f . fmap (foldFix f) . unfix

-- | Unfold 'Fix'.
--
-- >>> unfoldFix (\i -> if i < 4 then Cons i (i + 1) else Nil) (0 :: Int)
-- Fix (Cons 0 (Fix (Cons 1 (Fix (Cons 2 (Fix (Cons 3 (Fix Nil))))))))
--
unfoldFix :: Functor f => (a -> f a) -> a -> Fix f
unfoldFix f = Fix . fmap (unfoldFix f) . f

-------------------------------------------------------------------------------
-- Functor instances
-------------------------------------------------------------------------------

instance Eq1 f => Eq (Fix f) where
    Fix a == Fix b = eq1 a b

instance Ord1 f => Ord (Fix f) where
    compare (Fix a) (Fix b) = compare1 a b

instance Show1 f => Show (Fix f) where
    showsPrec d (Fix a) =
        showParen (d >= 11)
            $ showString "Fix "
            . showsPrec1 11 a

#ifdef __GLASGOW_HASKELL__
instance Read1 f => Read (Fix f) where
    readPrec = parens $ prec 10 $ do
        Ident "Fix" <- lexP
        fmap Fix (step (readS_to_Prec readsPrec1))
#endif

-------------------------------------------------------------------------------
-- hashable
-------------------------------------------------------------------------------

instance Hashable1 f => Hashable (Fix f) where
    hashWithSalt salt = hashWithSalt1 salt . unfix

#if MIN_VERSION_deepseq(1,4,3)
instance NFData1 f => NFData (Fix f) where
    rnf = rnf1 . unfix
#endif

-------------------------------------------------------------------------------
-- Typeable and Data
-------------------------------------------------------------------------------

#ifdef __GLASGOW_HASKELL__
#if HAS_POLY_TYPEABLE
deriving instance Typeable Fix
deriving instance (Typeable f, Data (f (Fix f))) => Data (Fix f)
#else
instance Typeable1 f => Typeable (Fix f) where
   typeOf t = mkTyConApp fixTyCon [typeOf1 (undefined `asArgsTypeOf` t)]
     where asArgsTypeOf :: f a -> Fix f -> f a
           asArgsTypeOf = const

fixTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
fixTyCon = mkTyCon3 "recursion-schemes" "Data.Functor.Foldable" "Fix"
#else
fixTyCon = mkTyCon "Data.Functor.Foldable.Fix"
#endif
{-# NOINLINE fixTyCon #-}

instance (Typeable1 f, Data (f (Fix f))) => Data (Fix f) where
  gfoldl f z (Fix a) = z Fix `f` a
  toConstr _ = fixConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z (Fix))
    _ -> error "gunfold"
  dataTypeOf _ = fixDataType

fixConstr :: Constr
fixConstr = mkConstr fixDataType "Fix" [] Prefix

fixDataType :: DataType
fixDataType = mkDataType "Data.Functor.Foldable.Fix" [fixConstr]
#endif
#endif

-------------------------------------------------------------------------------
-- Mu
-------------------------------------------------------------------------------

newtype Mu f = Mu (forall a. (f a -> a) -> a)

instance (Functor f, Eq1 f) => Eq (Mu f) where
    (==) = (==) `on` foldMu Fix

instance (Functor f, Ord1 f) => Ord (Mu f) where
    compare = compare `on` foldMu Fix

instance (Functor f, Show1 f) => Show (Mu f) where
    showsPrec d f = showParen (d > 10) $
        showString "unfoldMu unfix " . showsPrec 11 (foldMu Fix f)

#ifdef __GLASGOW_HASKELL__
instance (Functor f, Read1 f) => Read (Mu f) where
    readPrec = parens $ prec 10 $ do
        Ident "unfoldMu" <- lexP
        Ident "unfix" <- lexP
        fmap (unfoldMu unfix) (step readPrec)
#endif

hoistMu :: (forall a. f a -> g a) -> Mu f -> Mu g
hoistMu n (Mu mk) = Mu $ \roll -> mk (roll . n)

-- | Fold 'Mu'.
--
-- >>> let mu = unfoldMu (\i -> if i < 4 then Cons i (i + 1) else Nil) (0 :: Int)
-- >>> foldMu (elimListF 0 (+)) mu
-- 6
foldMu :: (f a -> a) -> Mu f -> a
foldMu f (Mu mk) = mk f

-- | Unfold 'Mu'.
--
-- >>> unfoldMu (\i -> if i < 4 then Cons i (i + 1) else Nil) (0 :: Int)
-- unfoldMu unfix (Fix (Cons 0 (Fix (Cons 1 (Fix (Cons 2 (Fix (Cons 3 (Fix Nil)))))))))
unfoldMu :: Functor f => (a -> f a) -> a -> Mu f
unfoldMu f x = Mu $ \mk -> hylo mk f x

-------------------------------------------------------------------------------
-- Nu
-------------------------------------------------------------------------------

data Nu f where Nu :: (a -> f a) -> a -> Nu f

instance (Functor f, Eq1 f) => Eq (Nu f) where
    (==) = (==) `on` foldNu Fix

instance (Functor f, Ord1 f) => Ord (Nu f) where
    compare = compare `on` foldNu Fix

instance (Functor f, Show1 f) => Show (Nu f) where
    showsPrec d f = showParen (d > 10) $
        showString "unfoldNu unfix " . showsPrec 11 (foldNu Fix f)

#ifdef __GLASGOW_HASKELL__
instance (Functor f, Read1 f) => Read (Nu f) where
    readPrec = parens $ prec 10 $ do
        Ident "unfoldNu" <- lexP
        Ident "unfix" <- lexP
        fmap (unfoldNu unfix) (step readPrec)
#endif

hoistNu :: (forall a. f a -> g a) -> Nu f -> Nu g
hoistNu n (Nu next seed) = Nu (n . next) seed

-- | Fold 'Nu'.
--
-- >>> let nu = unfoldNu (\i -> if i < 4 then Cons i (i + 1) else Nil) (0 :: Int)
-- >>> foldNu (elimListF 0 (+)) nu
-- 6
--
foldNu :: Functor f => (f a -> a) -> Nu f -> a
foldNu f (Nu next seed) = hylo f next seed

-- | Unfold 'Nu'.
--
-- >>> unfoldNu (\i -> if i < 4 then Cons i (i + 1) else Nil) (0 :: Int)
-- unfoldNu unfix (Fix (Cons 0 (Fix (Cons 1 (Fix (Cons 2 (Fix (Cons 3 (Fix Nil)))))))))
unfoldNu :: (a -> f a) -> a -> Nu f
unfoldNu = Nu

-------------------------------------------------------------------------------
-- hylo, not exported
-------------------------------------------------------------------------------

hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo f g = h where h = f . fmap h . g
