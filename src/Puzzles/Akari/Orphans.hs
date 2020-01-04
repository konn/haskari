{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Puzzles.Akari.Orphans () where
import           Data.Function
import           Data.Vector      (Vector)
import qualified Data.Vector      as V
import           Ersatz.Codec
import           Ersatz.Equatable

instance Codec a => Codec (Vector a) where
  type Decoded (Vector a) = Vector (Decoded a)
  decode s = V.mapM (decode s)
  encode = V.map encode

instance Equatable a => Equatable (Vector a) where
  (===) = (===) `on` V.toList
  {-# INLINE (===) #-}
  (/==) = (/==) `on` V.toList
  {-# INLINE (/==) #-}
