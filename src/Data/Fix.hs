{-# LANGUAGE CPP                  #-}
{-# LANGUAGE Trustworthy          #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE UndecidableInstances #-}

#define HAS_POLY_TYPEABLE MIN_VERSION_base(4,7,0)

#if HAS_POLY_TYPEABLE
{-# LANGUAGE StandaloneDeriving   #-}
#endif

-- | Fixed point of a functor.
module Data.Fix (
    Fix (..),
    unfix,
) where

import           Data.Functor.Classes (Eq1, Ord1, Read1, Show1, compare1, eq1,
                                       readsPrec1, showsPrec1)
import           Data.Hashable        (Hashable (..))
import           Data.Hashable.Lifted (Hashable1, hashWithSalt1)
import           Data.Typeable        (Typeable)
import           Text.Read            (Lexeme (Ident), Read (..), lexP, parens,
                                       prec, readS_to_Prec, step)

#if MIN_VERSION_deepseq(1,4,3)
import           Control.DeepSeq      (NFData (..), NFData1, rnf1)
#endif

#if HAS_POLY_TYPEABLE
import           Data.Data (Data)
#else
import           Data.Data
#endif

-------------------------------------------------------------------------------
-- Type
-------------------------------------------------------------------------------

newtype Fix f = Fix (f (Fix f))

unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f

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
