{-# LANGUAGE DerivingVia, FlexibleContexts, FlexibleInstances   #-}
{-# LANGUAGE FunctionalDependencies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables         #-}
{-# LANGUAGE StandaloneDeriving, TypeApplications, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances                               #-}
-- | Provides an interface language between Haskell and Ersatz.
module Puzzles.Classes where
import           Data.Coerce
import           Data.Foldable       hiding (and, or)
import           Data.Function
import qualified Data.HashMap.Strict as HM
import           Data.HashSet        (HashSet)
import           Data.IntMap         (IntMap)
import qualified Data.IntMap.Strict  as IM
import           Data.Map            (Map)
import qualified Data.Map.Strict     as M
import           Data.Set            (Set)
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.Word
import           Ersatz
import           Ersatz.BitChar
import           Prelude             hiding (and, not, or, (&&), (||))

import Puzzles.Akari.Orphans ()

class Boolean b => EquatableIn b a | a -> b where
  {-# MINIMAL (~==) | (~/=) #-}
  (~==) :: a -> a -> b
  infix 4 ~==
  (~==) = fmap not . (~/=)
  (~/=) :: a -> a -> b
  infix 4 ~/=
  (~/=) = fmap not . (~==)

newtype WrapEquatable a = WrapEquatable { runWrapEquatable :: a }

instance Equatable a => EquatableIn Bit (WrapEquatable a) where
  (~==) = coerce $ (===) @a
  (~/=) = coerce $ (/==) @a

deriving via WrapEquatable Bit  instance EquatableIn Bit Bit
deriving via WrapEquatable Bits instance EquatableIn Bit Bits
deriving via WrapEquatable Bit8 instance EquatableIn Bit Bit8
deriving via WrapEquatable Bit7 instance EquatableIn Bit Bit7
deriving via WrapEquatable Bit6 instance EquatableIn Bit Bit6
deriving via WrapEquatable Bit5 instance EquatableIn Bit Bit5
deriving via WrapEquatable Bit4 instance EquatableIn Bit Bit4
deriving via WrapEquatable Bit3 instance EquatableIn Bit Bit3
deriving via WrapEquatable Bit2 instance EquatableIn Bit Bit2
deriving via WrapEquatable Bit1 instance EquatableIn Bit Bit1
deriving via WrapEquatable BitChar instance EquatableIn Bit BitChar

newtype WrapEq a = WrapEq { runWrapEq :: a }
  deriving newtype (Eq, Ord, Show)

instance (Eq a) => EquatableIn Bool (WrapEq a) where
  (~==) = fmap bool . coerce ((==) @a)
  {-# INLINE (~==) #-}
  (~/=) = fmap bool . coerce ((/=) @a)
  {-# INLINE (~/=) #-}

deriving via WrapEq Int
  instance EquatableIn Bool Int
deriving via WrapEq Bool
  instance EquatableIn Bool Bool
deriving via WrapEq Word
  instance EquatableIn Bool Word
deriving via WrapEq Word8
  instance EquatableIn Bool Word8
deriving via WrapEq Word16
  instance EquatableIn Bool Word16
deriving via WrapEq Word32
  instance EquatableIn Bool Word32

instance EquatableIn b a => EquatableIn b [a] where
  xs ~== ys = bool (length xs == length ys) && and (zipWith (~==) xs ys)
  {-# INLINE (~==) #-}
  xs ~/= ys = bool (length xs /= length ys) || or  (zipWith (~/=) xs ys)
  {-# INLINE (~/=) #-}

instance EquatableIn b a => EquatableIn b (Vector a) where
  (~==) = (~==) `on` V.toList
  (~/=) = (~/=) `on` V.toList

instance (Eq k, EquatableIn b v) => EquatableIn b (Map k v) where
  m ~== n
    | M.keys m == M.keys n = M.elems m ~== M.elems n
    | otherwise = false

instance (Eq k, EquatableIn b v) => EquatableIn b (HM.HashMap k v) where
  m ~== n
    | HM.keys m == HM.keys n = HM.elems m ~== HM.elems n
    | otherwise = false

instance EquatableIn b v => EquatableIn b (Set v) where
  (~==) = (~==) `on` toList

instance EquatableIn b v => EquatableIn b (HashSet v) where
  (~==) = (~==) `on` toList

instance (EquatableIn b v) => EquatableIn b (IntMap v) where
  m ~== n
    | IM.keys m == IM.keys n = IM.elems m ~== IM.elems n
    | otherwise = false
