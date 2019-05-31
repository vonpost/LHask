{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeOperators                         #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module Ops where
import Backprop
import Lift
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Exp(..)
                             ,Arrays(..)
                             ,Acc(..)
                             ,(:.)(..)
                             ,Z (..))
import Data.Array.Accelerate.System.Random.MWC
import qualified Prelude as P 
import Prelude hiding ((++), map, zipWith, (<>))
import Numeric.Backprop
import GHC.TypeLits
import Data.Proxy
import Data.List (foldl')
--------------------------------------------------------------------------------
class RandomArray a where
  sample :: IO (a)
instance (KnownNat n) => RandomArray (SR n) where
  sample = do a <- randomArray uniform (Z:.n) :: IO(A.Vector ℝ)
              return (pack @(SR n) a)
    where n = fromInteger $ natVal (Proxy @n) :: Int
  -- sample = return (run 0)
instance (KnownNat m, KnownNat n) => RandomArray (SL m n) where
  sample = do a <- randomArray uniform (Z:.m:.n) :: IO (A.Matrix ℝ)
              return (pack @(SL m n) a)
    where n = fromInteger $  natVal (Proxy @n) :: Int
          m = fromInteger $  natVal (Proxy @m) :: Int
instance (RandomArray a, RandomArray b) => RandomArray (a,b) where
  sample = do a <- sample
              b <- sample
              return (a,b)
--------------------------------------------------------------------------------

type Rate = Exp ℝ
type Learn p a b = forall z. (Backprop p, Backprop a, Backprop b, Reifies z W)
                 => BVar z p -> BVar z a -> BVar z b


-- Post-composition of learners
(~>) :: (Backprop p,
         Runable p,
         Backprop q,
         Runable q,
         Backprop b,
         Runable b,
         Reifies z W)
     =>  (BVar z p -> BVar z a -> BVar z b)
     ->  (BVar z q -> BVar z b -> BVar z c)
     ->  (BVar z (p ⊗ q) -> BVar z a -> BVar z c)
(~>) i j (p:*:q) = j q . compute . i p

(⊗) :: (Backprop p,
        Runable p,
        Backprop q,
        Runable q,
        Backprop a,
        Runable a,
        Backprop b,
        Runable b,
        Backprop c,
        Runable c,
        Backprop d,
        Runable d)
    => (Learn p a c)
    -> (Learn q b d)
    -> (Learn (p ⊗ q) (a ⊗ b) (c ⊗ d))
(⊗) i j (p:*:q) (a:*:b) = (i p a):*:(j q b)

type Error a b = forall z. (Backprop a, Backprop b, Reifies z W)
               => BVar z a -> BVar z a -> BVar z b
errorGrad :: (Backprop p,
              Backprop a,
              Backprop b,
              Runable c,
              Backprop c)
          => (forall z. Reifies z W => BVar z p -> BVar z a -> BVar z b)
          -> (forall s. Reifies s W => BVar s b -> BVar s b -> BVar s c)
          -> a
          -> b
          -> p
          -> p
errorGrad i e a b = gradBP $ \p -> e (i p $ auto a) (auto b)



logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

cmpr :: forall z.
        (Reifies z W)
     => (Exp ℝ -> Exp ℝ -> Exp Bool) 
     -> BVar z (Exp ℝ)
     -> BVar z (Exp ℝ)
     -> BVar z (Exp ℝ)
     
cmpr f = liftOp2 . op2 $ \x y -> (A.cond (f x y) x y,
                               (\d -> A.unlift $ A.cond (f x y)
                                      (A.T2 d 0)
                                      (A.T2 0 d)
                               ))
                              
  


konstR :: forall m z.
          (KnownNat m,
           Reifies z W) 
       => BVar z (R 1)
       -> BVar z (R m)
konstR = liftOp1 . op1 $ \(unpack -> x) -> let val = x A.!! 0
                                               m = fromInteger $ natVal (Proxy @m)
                                           in (pack $ A.fill (A.constant $ Z:.m) val,
                                               evalBP sumR)

replicateL :: forall n m k.
              (KnownNat n,
               KnownNat m)
           => (R n)
           -> (L m n)
replicateL (unpack -> v) = let m = fromInteger $ natVal (Proxy @m) :: Int
                           in pack $ A.replicate (A.constant $ Z:.m:.A.All) v
 
konstL :: forall n m z.
          (KnownNat n,
           KnownNat m,
           Reifies z W) 
       => BVar z (R m)
       -> BVar z (L m n)
konstL = liftOp1 . op1 $ \(unpack -> x) -> let n = fromInteger $ natVal (Proxy @n) :: Int
                                           in (pack $ A.replicate (A.constant $ Z:.A.All:.n) x,
                                               evalBP sumL)

sumL :: forall n m z.
        (KnownNat n,
         KnownNat m,
         Reifies z W)
     => BVar z (L m n)
     -> BVar z (R m)
sumL = liftOp1 . op1 $ \(L (Dim (Dim m))) -> (mkR $ A.fold (+) 0 m,
                                              (evalBP konstL))
sumAllL :: forall n m z.
        (KnownNat n,
         KnownNat m,
         Reifies z W)
     => BVar z (L m n)
     -> BVar z (R 1)
sumAllL = liftOp1 . op1 $ \(L (Dim (Dim m))) -> (pack $ A.flatten $ A.foldAll (+) 0 m,
                                                 (evalBP konstL) . (evalBP konstR))
-- WHAT A MONSTER. Could be HEAVILY simiplified, but w/e.
maximumR :: forall n z. (KnownNat n, Reifies z W) => BVar z (R n) -> BVar z (R 1)
maximumR = liftOp1 . op1 $ \x -> let (A.T2 (A.unindex1 -> i) max') =   
                                                    A.the . 
                                                      A.fold1 (\((A.T2 idx m))
                                                         (A.T2 idx' m') ->
                                                          A.cond (m A.>m') (A.T2 idx m) (A.T2 idx' m')
                                                       )
                                                    . A.indexed
                                                    . unpack $ x
                                     n = fromInteger $ natVal (Proxy @n)
                                 in (pack . A.flatten $ A.unit max',
                                     const .
                                     pack $
                                     A.generate (A.constant $ Z:.n) (\(A.unindex1 -> idx) ->
                                                                       A.cond (idx A.== i) 1 0)
                                    )
                                   
maximumL :: forall m n z. (KnownNat n, KnownNat m, Reifies z W) => BVar z (L m n) -> BVar z (R m)
maximumL = liftOp1 . op1 $ \x -> let (idxs,maxs) =  A.unzip .
                                                    A.fold1 (\(A.T2 idx m) (A.T2 idx' m') ->
                                                               A.cond (m A.> m') (A.T2 idx m) (A.T2 idx' m'))
                                                    . A.indexed
                                                    . unpack
                                                    $ x
                                     n = fromInteger $ natVal (Proxy @n) :: Int
                                     grad d = pack $
                                              A.imap (\idx (A.T2 idx' val) ->
                                                        A.cond ((A.unindex2 idx) A.== (A.unindex2 idx')) val 0.0)
                                              $ A.replicate (A.constant $ Z:. A.All :. n)
                                              $ A.zip idxs (unpack d) 
                                 in (pack maxs,
                                     \d -> grad d)
                                     
softMaxR :: (Reifies z W, KnownNat n) => BVar z (R n) -> BVar z (R n)
softMaxR x = expx / (konstR $ sumR expx) 
  where
    expx = exp x

transpose :: (Reifies z W, KnownNat m, KnownNat n)
          => BVar z (L m n)
          -> BVar z (L n m)
transpose = isoVar (pack . A.transpose . unpack) (pack . A.transpose . unpack)

softMaxL :: forall n m z.
            (Reifies z W,
             KnownNat n,
             KnownNat m)
         => BVar z (L m n)
         -> BVar z (L m n)
softMaxL x = expx / (konstL $ sumL expx) 
  where
    expx = exp x
linReg :: (KnownNat n, KnownNat m, Reifies z W)
       => BVar z ((L m n) ⊗ (R m))
       -> BVar z (R n)
       -> BVar z (R m)
linReg (w:*:b) v = w #> v + b

squaredErrorR :: forall n z. (KnownNat n, Reifies z W)
             => BVar z (R n) -> BVar z (R n) -> BVar z (R 1) 
squaredErrorR x y = (1/n)*(sumR (x-y)**2)
  where n = fromInteger $ natVal (Proxy @n) 

crossEntropyR :: (KnownNat n, Reifies z W)
             => BVar z (R n) -> BVar z (R n) -> BVar z (R 1) 
crossEntropyR x y = negate . sumR $ y * (log x)
clipBy :: Reifies z W
       => BVar z (Exp ℝ)
       -> BVar z (Exp ℝ)
       -> BVar z (Exp ℝ)
clipBy epsilon x = cmpr (A.<) (cmpr (A.>) x epsilon) (1-epsilon)
crossEntropyL :: forall m n z. (KnownNat m, KnownNat n, Reifies z W)
              => BVar z (L m n) -> BVar z (L m n) -> BVar z (R 1) 
              -- => Error (L m n) (R 1)
crossEntropyL x y = (/ m) . negate . sumAllL $ y * (log $ mapB (clipBy 1e-7) x)
  where m = fromInteger $ natVal (Proxy @m)

squaredErrorL :: forall m n z. (KnownNat n, KnownNat m, Reifies z W)
             => BVar z (L m n)
             -> BVar z (L m n)
             -> BVar z (R 1)
squaredErrorL x y = sumR $ 1/n*(sumL ((x-y)*(x-y)))
  where n = fromInteger $ natVal (Proxy @n) 
        m = fromInteger $ natVal (Proxy @m)


fullyConnected :: (KnownNat n, KnownNat m, Reifies z W)
               => (BVar z (R m) -> BVar z (R m)) 
               -> BVar z ((L m n) ⊗ (R m))
               -> BVar z (R n)
               -> BVar z (R m)
fullyConnected act (w :*: b) v = act $ (w #> v)+b

fullyConnectedL :: (KnownNat n, KnownNat m, KnownNat k, Reifies z W)
               => (BVar z (L m k) -> BVar z (L m k)) 
               -> BVar z ((L n k) ⊗ (L m k))
               -> BVar z (L m n)
               -> BVar z (L m k)
fullyConnectedL act (w :*: b) v = act $ (v <> w)+b
----------------------------------------------------------------------------------

