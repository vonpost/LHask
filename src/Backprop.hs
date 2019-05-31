{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeOperators                         #-}
{-# LANGUAGE PartialTypeSignatures                         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ViewPatterns                         #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

-- Because of the tuple type.. We could avoid it by unpacking into accs instead of static accs.
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures     #-}

module Backprop where
import Lift
import Numeric.Backprop
import Numeric.Backprop.Class
import GHC.TypeLits
import Control.DeepSeq
import Data.Array.Accelerate.IO.Data.ByteString
import qualified Data.Array.Accelerate.Numeric.LinearAlgebra as N
import Data.Array.Accelerate.Numeric.LinearAlgebra.BLAS.Level3 as N
import Data.Array.Accelerate (transpose,
                              fold,
                              flatten,
                              Acc,
                              Arrays,
                              (:.)(..),
                              Z(..),
                              Exp,
                              constant,
                             )
import qualified Data.Array.Accelerate as A
import Prelude hiding (replicate, map, zipWith, (++))
import Data.Proxy
import Control.Arrow ((***), (&&&))
import Control.Lens (Lens', lens)
import qualified Data.Array.Accelerate.LLVM.PTX as LLVM
import Data.Binary
import GHC.Generics (Generic)
import Data.Coerce
instance Backprop (Exp ℝ) where
  zero = zeroNum
  one = oneNum
  add = addNum
instance (KnownNat n) => Backprop (R n) where
  zero = zeroNum 
  add = addNum
  one = oneNum
instance (KnownNat n, KnownNat m) => Backprop (L n m) where
  zero = zeroNum
  add = addNum
  one = oneNum
-----
--- CLEAN UP WHEN TIME

newtype SR n = SR (Dim n (A.Vector ℝ)) deriving (Generic, Show, NFData)

-- UNSAFE. We should put a word8 indicating lengths and match against supposed type.
instance (KnownNat n) => Binary (SR n) where
  put = put . toByteStrings . unpack
  get = do byteArr <- get
           let n = fromInteger $ natVal (Proxy @n)
           return $ pack $ fromByteStrings (Z:.n) byteArr
newtype SL m n = SL (Dim m (Dim n (A.Matrix ℝ))) deriving (Generic, Show, NFData)

instance (KnownNat m, KnownNat n) => Binary (SL m n) where
  put = put . toByteStrings . unpack
  get = do byteArr <- get
           let n = fromInteger $ natVal (Proxy @n)
               m = fromInteger $ natVal (Proxy @m)
           return . pack . fromByteStrings (Z:.m:.n) $ byteArr
newtype a ⊗ b = Tpl (Acc (Unpack (Run a), Unpack (Run b))) 

instance (Backprop a,
          Backprop b,
          Runable a,
          Runable b) => Backprop (a ⊗ b) where
  add (a:::b) (a':::b') = (add a a'):::(add b b')
  one (a:::b) = (one a):::(one b)
  zero (a:::b) = (zero a):::(zero b)
  
_Packed :: (Packable a) => Lens' a (Unpack a)
_Packed = lens unpack (\_ b -> pack b)

_fst :: (Runable a, Runable b) => Lens' (a ⊗ b) a
_fst = lens (\(a ::: _) -> a) (\(_ ::: b) a' -> (a' ::: b)) 

_snd :: (Runable a, Runable b) => Lens' (a ⊗ b) b
_snd = lens (\(_ ::: b) -> b) (\(a ::: _) b' -> (a ::: b'))

unliftP :: (Packable a, Packable b) =>
           (b -> a) -> Unpack b -> Unpack a
unliftP f = unpack . f . pack

liftP :: (Packable c, Packable a) =>
         (Unpack a -> Unpack c) -> a -> c
liftP f = pack . f . unpack 
run1 :: forall a b. (Runable a, Runable b) => (a -> b) -> (Run a -> Run b)
run1 = liftP . LLVM.run1 . unliftP 

-- Bidirectional pattern for constructing and deconstructing tensors
pattern (:::) ::  (Runable a,
                   Runable b)
              => a
              -> b
              -> a ⊗ b
pattern a ::: b <- (unpack -> (A.afst &&& A.asnd) -> pack *** pack -> (a,b))
  where a ::: b = Tpl (A.T2 (unpack a) (unpack b))
{-# COMPLETE (:::) #-}
  
-- Bidirectional pattern for constructing and deconstructing BVars of tensors. Useful for pattern matching.
pattern (:*:) :: (Backprop a,
                  Backprop b,
                  Runable a,
                  Runable b,
                  Reifies z W)
              =>  BVar z a
              ->  BVar z b
              ->  BVar z (a ⊗ b)
pattern a :*: b <- (\ab -> (ab ^^. _fst, ab ^^. _snd) -> (a,b))
  where (:*:) = isoVar2 (:::) (\(a:::b) -> (a,b))
{-# COMPLETE (:*:) #-}

class (Packable a,
       Packable (Run a),
       Arrays (Unpack (Run a)),
       Unpack a ~ Acc (Unpack (Run a))) => Runable a where
  type Run a = result | result -> a
  run :: a -> Run a
  run = pack . LLVM.run . unpack
  use :: Run a -> a
  use = pack . A.use . unpack

instance Runable (R n) where
  type Run (R n) = SR n
  
instance Runable (L m n)  where
  type Run (L m n) = SL m n

instance (Runable a,
          Runable b)
       => Runable (a ⊗ b) where
  type Run (a ⊗ b) = ((Run a), (Run b))

-- This class defines how we can get out of the dependently typed context.
class (Coercible (Unpack a) a) => Packable a where
  type Unpack a 
  pack   :: Unpack a -> a
  pack = coerce
  unpack = coerce
  unpack :: a -> Unpack a

  
instance Packable (R n) where
  type Unpack (R n) = (Acc (A.Vector ℝ))
  
instance Packable (SR n)  where
  type Unpack (SR n) = (A.Vector ℝ)

instance Packable (L m n)  where
  type Unpack (L m n) = (Acc (A.Matrix ℝ))

instance Packable (SL m n) where
  type Unpack (SL m n)  = (A.Matrix ℝ)

instance (Runable a, Runable b) => Packable (a ⊗ b) where
  type Unpack (a ⊗ b) = Acc (Unpack (Run a), Unpack (Run b))

instance (Packable a, Packable b) => Packable (a, b) where
  type Unpack (a, b) = (Unpack a, Unpack b)

class (Runable a, Backprop a) => Static a where
  map :: (A.Exp ℝ -> A.Exp ℝ) -> a -> a
  zipWith :: (A.Exp ℝ -> A.Exp ℝ -> A.Exp ℝ) -> a -> a -> a

instance (KnownNat n, KnownNat m) => Static (L m n) where
  map f (L (Dim (Dim m))) = mkL . (A.map f) $ m
  zipWith f (L (Dim (Dim v))) (L (Dim (Dim w))) = mkL $ A.zipWith f v w

instance (KnownNat n) => Static (R n) where
  map f (R (Dim v)) = mkR . (A.map f) $ v
  zipWith f (R (Dim v)) (R (Dim w)) = mkR $ A.zipWith f v w

instance (Static a, Static b) => Static (a ⊗ b) where
  map f (a ::: b) = (map f a) ::: (map f b)
  zipWith f (a ::: b) (a' ::: b') = (zipWith f a a') ::: (zipWith f b b')
  
class (Runable p, Backprop p) => Parameter p  where
  update :: (Exp ℝ) -> p -> p -> p
  default update :: (Num p, Static p) => (Exp ℝ) -> p -> p -> p
  update rate p gradient = p-(map (* rate)) gradient

instance (KnownNat n) => Parameter (R n) where

instance (KnownNat n, KnownNat m) => Parameter (L m n) 

instance (Parameter a, Parameter b) => Parameter (a ⊗ b) where
  update rate (p1:::p2) (g1:::g2) = let a' = update rate p1 g1
                                        b' = update rate p2 g2
                                    in (a':::b')
--------------------------------------------------------------------------------

(#>) :: (KnownNat m, KnownNat n, Reifies z W)
     => BVar z (L m n)
     -> BVar z (R n)
     -> BVar z (R m)
(#>) = liftOp2 . op2 $ \(L (Dim (Dim m))) (R (Dim v)) -> (mkR $ m N.#> v,
                                                          \(R (Dim d)) -> (mkL $ d N.>< v,
                                                                           mkR $ (transpose m) N.#> d)
                                                         )

(<>) :: (KnownNat m, KnownNat n, KnownNat k, Reifies z W)
     => BVar z (L m n)
     -> BVar z (L n k)
     -> BVar z (L m k)
(<>) = liftOp2 . op2 $ \(unpack -> m1)
                        (unpack -> m2) -> (pack $ m1 N.<> m2,
                                           \(unpack -> d)
                                           -> (pack $ gemm 1 N d T m2,
                                               pack $ gemm 1 T m1 N d)
                                          )

-- Not sure of fold's gradient, so sum for now.
sumR :: forall n z.
        (KnownNat n,
         Reifies z W)
     => BVar z (R n)
     -> BVar z (R 1)
sumR = liftOp1 . op1 $ \(R (Dim v)) -> (mkR . flatten $ fold (+) 0 v,
                                        \(R (Dim d)) -> let val = d A.!! 0
                                                            n = fromInteger $ natVal (Proxy @n) :: Int
                                                        in mkR $ A.fill (constant $ Z:.n) val
                                       )
       
mapB :: (Static tensor,
         Num tensor,
         Reifies z W)
     => (forall s. Reifies s W => BVar s (Exp ℝ) -> BVar s (Exp ℝ))
     -> BVar z tensor
     -> BVar z tensor
mapB f = liftOp1 . op1 $ \t -> (map (evalBP f) t,
                                 \d -> d * (map (gradBP f) t))



compute :: (Runable a, Backprop a, Reifies z W) => BVar z a -> BVar z a
compute = isoVar (pack . A.compute . unpack) id

zipWithB :: (Static tensor,
             Num tensor,
             KnownNat n,
             KnownNat m,
             Reifies z W)
         => (forall s. Reifies s W => BVar s (Exp ℝ) -> BVar s (Exp ℝ) -> BVar s (Exp ℝ))
         -> BVar z tensor
         -> BVar z tensor
         -> BVar z tensor
zipWithB f = liftOp2 . op2 $ \v w
                             -> (zipWith (evalBP2 f) v w,
                                  (\d ->
                                      let dx = zipWith (\x -> fst . gradBP2 f x) v w
                                          dy = zipWith (\x -> snd . gradBP2 f x) v w
                                      in (d * dx, d * dy)
                                  )
                                )

(++) :: forall n m z.
        (KnownNat n,
         KnownNat m,
         Reifies z W)
     => BVar z (R n)
     -> BVar z (R m)
     -> BVar z (R (n+m))
(++) = liftOp2 . op2 $ \(R (Dim v)) (R (Dim w)) -> (mkR (v A.++ w),
                                                    \(R (Dim d)) -> let n = constant .
                                                                            fromInteger .
                                                                            natVal $
                                                                            (Proxy @n) :: Exp Int
                                                                    in (mkR $ A.take n d,
                                                                        mkR $ A.drop n d)
                                                   )
-- Doesn't work atm because of bool not Bvar.
-- cond :: (Reifies z W) => Exp Bool -> BVar z (Exp  ℝ) ->  (BVar z (Exp  ℝ) -> BVar z (Exp ℝ))
-- cond bool = liftOp2 . op2 $ \x y -> (A.cond bool x y,
--                                      const (A.cond bool 1 0, A.cond bool 0 1))



relu :: (Reifies z W)
     => BVar z (Exp  ℝ)
     -> BVar z (Exp  ℝ)
relu = liftOp1 . op1 $ \x -> (A.cond (x A.> 0) x 0,
                              \d -> A.cond (x A.> 0) d 0)

