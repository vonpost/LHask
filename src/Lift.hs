{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Lift where
import GHC.TypeLits
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate ( Z(..), (:.)(..))
import Control.Category
import Control.DeepSeq
import Prelude hiding ((.))
import Numeric.Backprop
import Data.Proxy (Proxy(..))
import Data.Array.Accelerate.Numeric.LinearAlgebra hiding (Vector, Matrix)
import GHC.Generics (Generic)
import Data.Binary
--------------------------------------------------------------------------------
type ℝ = Float 
type Vector a = A.Acc (A.Vector a)
type Matrix a = A.Acc (A.Matrix a)

newtype Dim (n :: Nat) t = Dim t
  deriving (Show, Generic, Binary, NFData)

lift1F
  :: (c t -> c t)
  -> Dim n (c t) -> Dim n (c t)
lift1F f (Dim v) = Dim (f v)

lift2F
  :: (c t -> c t -> c t)
  -> Dim n (c t) -> Dim n (c t) -> Dim n (c t)
lift2F f (Dim u) (Dim v) = Dim (f u v)
--------------------------------------------------------------------------------

newtype R n = R (Dim n (Vector ℝ))
  deriving (Generic, Num, Fractional, Floating)
newtype L m n = L (Dim m (Dim n (Matrix ℝ)))
  deriving (Generic)

mkL :: Matrix ℝ -> L m n
mkL x = L (Dim (Dim x))

mkR :: Vector ℝ -> R n
mkR = R . Dim
--------------------------------------------------------------------------------

type V n t = Dim n (Vector t)

ud :: Dim n (Vector t) -> Vector t
ud (Dim v) = v

mkV :: forall (n :: Nat) t . t -> Dim n t
mkV = Dim

instance (KnownNat n, Num (A.Exp t), Numeric t) => Num (Dim n (Vector t)) where
  (+) = lift2F $ A.zipWith (A.+)
  (*) = lift2F $ A.zipWith (A.*)
  (-) = lift2F $ A.zipWith (A.-)
  abs = lift1F $ A.map abs
  signum = lift1F $ A.map signum
  negate = lift1F $ A.map (A.* (-1))
  fromInteger = Dim . A.fill (A.constant $ Z:.n) . A.fromInteger 
    where n = fromInteger $ natVal (Proxy @n) :: Int

instance (KnownNat n, Num (A.Exp t), Fractional (A.Exp t), Numeric t) => Fractional (Dim n (Vector t))
  where
    fromRational = Dim . A.fill (A.constant $ Z:.n) . A.fromRational
      where n = fromInteger $ natVal (Proxy @n) :: Int
    (/) = lift2F $ A.zipWith (A./)
instance (KnownNat n, Fractional (A.Exp t), Floating (A.Exp t), Numeric t) => Floating (Dim n (Vector t)) where
    sin   = lift1F $ A.map sin
    cos   = lift1F $ A.map cos
    tan   = lift1F $ A.map tan
    asin  = lift1F $ A.map asin
    acos  = lift1F $ A.map acos
    atan  = lift1F $ A.map atan
    sinh  = lift1F $ A.map sinh
    cosh  = lift1F $ A.map cosh
    tanh  = lift1F $ A.map tanh
    asinh = lift1F $ A.map asinh
    acosh = lift1F $ A.map acosh
    atanh = lift1F $ A.map atanh
    exp   = lift1F $ A.map exp
    log   = lift1F $ A.map log
    sqrt  = lift1F $ A.map sqrt
    (**)  = lift2F $ A.zipWith (**)
    pi = Dim . A.fill (A.constant $ Z:.n) $ A.pi
      where n = fromInteger $ natVal (Proxy @n) :: Int

--------------------------------------------------------------------------------
instance (KnownNat n, KnownNat m, Num (A.Exp t), Numeric t) => Num (Dim m (Dim n (Matrix t)))
  where
    (+) = (lift2F . lift2F) $ A.zipWith (+)
    (*) = (lift2F . lift2F) $ A.zipWith (*)
    (-) = (lift2F . lift2F) $ A.zipWith (-)
    abs = (lift1F . lift1F) $ A.map abs
    signum = (lift1F . lift1F) $ A.map signum
    negate = (lift1F . lift1F) $ A.map negate
    fromInteger = Dim . Dim . A.fill (A.constant $ Z:.m:.n) . A.fromInteger
      where n = fromInteger $ natVal (Proxy @n) :: Int
            m = fromInteger $ natVal (Proxy @m) :: Int


instance (KnownNat n, KnownNat m, Fractional (A.Exp t), Numeric t) => Fractional (Dim m (Dim n (Matrix t)))
  where
    fromRational = Dim . Dim . A.fill (A.constant $ Z:.m:.n) . A.fromRational
      where n = fromInteger $ natVal (Proxy @n) :: Int
            m = fromInteger $ natVal (Proxy @m) :: Int
    (/) = (lift2F.lift2F) $ A.zipWith (/)

instance (KnownNat n, KnownNat m, Fractional (A.Exp t), Floating (A.Exp t), Numeric t) => Floating (Dim m (Dim n (Matrix t))) where
    sin   = (lift1F . lift1F) $ A.map sin
    cos   = (lift1F . lift1F) $ A.map cos
    tan   = (lift1F . lift1F) $ A.map tan
    asin  = (lift1F . lift1F) $ A.map asin
    acos  = (lift1F . lift1F) $ A.map acos
    atan  = (lift1F . lift1F) $ A.map atan
    sinh  = (lift1F . lift1F) $ A.map sinh
    cosh  = (lift1F . lift1F) $ A.map cosh
    tanh  = (lift1F . lift1F) $ A.map tanh
    asinh = (lift1F . lift1F) $ A.map asinh
    acosh = (lift1F . lift1F) $ A.map acosh
    atanh = (lift1F . lift1F) $ A.map atanh
    exp   = (lift1F . lift1F) $ A.map exp
    log   = (lift1F . lift1F) $ A.map log
    sqrt  = (lift1F . lift1F) $ A.map sqrt
    (**)  = (lift2F . lift2F) $ A.zipWith (**)
    pi    = Dim . Dim . A.fill (A.constant $ Z:.m:.n) $ pi
      where n = fromInteger $ natVal (Proxy @n) :: Int
            m = fromInteger $ natVal (Proxy @m) :: Int
 
--------------------------------------------------------------------------------
lift1L f (L v) = L (f v)
lift2L f (L a) (L b) = L (f a b)
instance (KnownNat n, KnownNat m) =>  Num (L n m)
  where
    (+) = lift2L (+)
    (*) = lift2L (*)
    (-) = lift2L (-)
    abs = lift1L abs
    signum = lift1L signum
    negate = lift1L negate
    fromInteger = L . fromInteger

instance (KnownNat n, KnownNat m) => Fractional (L n m)
  where
    fromRational =  L . fromRational 
    (/) = lift2L (/)

instance (KnownNat n, KnownNat m) => Floating (L n m) where
    sin   = lift1L sin
    cos   = lift1L cos
    tan   = lift1L tan
    asin  = lift1L asin
    acos  = lift1L acos
    atan  = lift1L atan
    sinh  = lift1L sinh
    cosh  = lift1L cosh
    tanh  = lift1L tanh
    asinh = lift1L asinh
    acosh = lift1L acosh
    atanh = lift1L atanh
    exp   = lift1L exp
    log   = lift1L log
    sqrt  = lift1L sqrt
    (**)  = lift2L (**)
    pi    = L pi


instance (KnownNat n) => Show (R n) where
  show (R (Dim a)) =   "ℝ " ++ show (natVal (undefined :: Proxy n)) ++ " " ++ show a
