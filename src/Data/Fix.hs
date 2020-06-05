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

-- | Fixed point of a functor.
module Data.Fix (
    -- * Fix
    Fix (..),
    unfix,
    hoistFix,
    hoistFix',
    cata,
    ana,
    hylo,
    -- * Mu - least fixed point
    Mu (..),
    hoistMu,
    -- * Nu - greates fixed point
    Nu (..),
    hoistNu,
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

-- | Catamorphism or generic function fold.
cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unfix

-- | Anamorphism or generic function unfold.
ana :: Functor f => (a -> f a) -> a -> Fix f
ana f = Fix . fmap (ana f) . f

-- | Hylomorphism is anamorphism followed by catamorphism.
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo f g = h where h = f . fmap h . g

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

instance Read1 f => Read (Fix f) where
    readPrec = parens $ prec 10 $ do
        Ident "Fix" <- lexP
        fmap Fix (step (readS_to_Prec readsPrec1))

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
    (==) = (==) `on` toFix

instance (Functor f, Ord1 f) => Ord (Mu f) where
    compare = compare `on` toFix

instance (Functor f, Show1 f) => Show (Mu f) where
    showsPrec d f = showParen (d > 10) $
        showString "fromFix " . showsPrec 11 (toFix f)

#ifdef __GLASGOW_HASKELL__
instance (Functor f, Read1 f) => Read (Mu f) where
    readPrec = parens $ prec 10 $ do
        Ident "fromFix" <- lexP
        fmap fromFix (step readPrec)
#endif

hoistMu :: (forall a. f a -> g a) -> Mu f -> Mu g
hoistMu n (Mu mk) = Mu $ \roll -> mk (roll . n)

-------------------------------------------------------------------------------
-- Nu
-------------------------------------------------------------------------------

data Nu f where Nu :: (a -> f a) -> a -> Nu f

instance (Functor f, Eq1 f) => Eq (Nu f) where
    (==) = (==) `on` toFix

instance (Functor f, Ord1 f) => Ord (Nu f) where
    compare = compare `on` toFix

instance (Functor f, Show1 f) => Show (Nu f) where
    showsPrec d f = showParen (d > 10) $
        showString "fromFix " . showsPrec 11 (toFix f)

#ifdef __GLASGOW_HASKELL__
instance (Functor f, Read1 f) => Read (Nu f) where
    readPrec = parens $ prec 10 $ do
        Ident "fromFix" <- lexP
        fmap fromFix (step readPrec)
#endif

hoistNu :: (forall a. f a -> g a) -> Nu f -> Nu g
hoistNu n (Nu next seed) = Nu (n . next) seed

-------------------------------------------------------------------------------
-- IsFix, not exported
-------------------------------------------------------------------------------

class IsFix fix where
    toFix   :: Functor f => fix f -> Fix f
    fromFix :: Functor f => Fix f -> fix f

instance IsFix Fix where
    toFix   = id
    fromFix = id

instance IsFix Mu where
    toFix (Mu f) = f Fix
    fromFix f    = Mu $ flip cata f

instance IsFix Nu where
    toFix (Nu f x) = ana f x
    fromFix        = Nu unfix
