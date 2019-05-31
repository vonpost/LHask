{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MNIST where
import Ops 
import Lift 
import Backprop as AB
import Data.IDX
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z(..),(:.)(..) )
import qualified Data.Vector.Unboxed as VU
import Prelude as P
import Control.Monad (forM_, forM)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State.Lazy as S
import Control.Monad.IO.Class
import Control.Arrow ((***))
import Data.Tuple (swap)
import Data.Proxy 
import Numeric.Backprop
import GHC.TypeLits
import System.Directory
import Data.Binary
import System.IO

-- Temporary chunksof code
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = P.map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n
-- 
-- Stolen and modified from JLE
loadMNISTB  :: forall m. KnownNat m
            => FilePath
            -> FilePath
            -> IO (Maybe [(SL m 784, SL m 10)])
loadMNISTB fpI fpL = runMaybeT $ do i <- MaybeT          $ decodeIDXFile       fpI
                                    l <- MaybeT          $ decodeIDXLabelsFile fpL
                                    d <- MaybeT . return $ labeledIntData l i
                                    return $ P.map (swap . (mkLabel *** mkImage) . P.unzip) . P.init $ chunksOf m d
                                      where 
                                        m = fromInteger $ natVal (Proxy @m) :: Int
                                        mkImage :: [VU.Vector Int] -> (SL m 784)
                                        mkImage =  pack . A.fromList (Z:.m:.784) . VU.toList . VU.map (\i -> fromIntegral i / 255) . VU.concat
                                        mkLabel :: [Int] -> (SL m 10)
                                        mkLabel =  pack . A.fromList (Z:.m:.10) . P.concat . P.map (\n -> P.map (\x -> if (x==n) then 1.0 else 0.0) [0..9])
-- 



writeMNIST :: forall m. (KnownNat m) => IO(Maybe [(SL m 784, SL m 10)])
writeMNIST = do let m = natVal (Proxy @m)
                    path = "mnist" P.++ "b" P.++ (show m) P.++ ".hldata"
                bool <- doesFileExist path
                if(bool) then decodeFile path >>= return
                  else
                  do
                  mnist <- loadMNISTB "data/train-images-idx3-ubyte" "data/train-labels-idx1-ubyte"
                  encodeFile path mnist
                  return mnist


type Accuracy = Float
type Loss = Float
type Dataset a b = [(a, b)]
type Epochs = Int
data TrainState p a b = TrainState (TrainConfig p a b) !Accuracy !Loss !p 
data TrainConfig p a b = TrainConfig Epochs (Dataset a b) (p -> (a,b) -> (SR 2, p))


sgd :: (Parameter p,
        Static a,
        Static b)
    =>  Rate
    -> (Learn p a b)
    -> (Error b (R 1))
    -> (b -> b -> R 1) 
    -> (Run p -> (Run a, Run b) -> (SR 2, Run p))
sgd rate learn err metric  = curry $ run1 sgdAcc 
  where sgdAcc (p:::(a:::b)) = 
          let newParams = update rate p grad 
              grad = errorGrad learn err a b p
              eval = evalBP2 learn newParams a
              loss = evalBP2 err eval b 
              acc = metric eval b
              lossacc = evalBP2 (AB.++) acc loss
          in lossacc:::newParams



xtract :: SR 2 -> (Accuracy, Accuracy)
xtract =  (\[a,b] -> (a,b)) . A.toList . unpack
trainOverBatches :: forall h m n k.
                    (KnownNat h,
                     KnownNat m,
                     KnownNat n,
                     KnownNat k)
                 => StateT (TrainState ((SL n h, SL m h), (SL h k, SL m k))
                             (SL m n) (SL m k))
                    IO ()
trainOverBatches  =
  do
    (TrainState (TrainConfig epochs dataset opt) _ _ _) <- S.get
    let target = "GPU"
        bsize = natVal (Proxy @m)
        prefix = "hl" P.++ target
        suffix = "b" P.++ (show bsize) P.++ "e" P.++ (show epochs) P.++ ".data"
    accFile <- liftIO $ openFile (prefix P.++ "acc" P.++ suffix) WriteMode
    lossFile <- liftIO $ openFile (prefix P.++ "loss" P.++ suffix) WriteMode
    forM_ [1..epochs] $ \e ->
      -- Iterate over epochs
      do
        liftIO $ putStrLn $ "Epoch " P.++ (show e)
        S.modify (\(TrainState tc _ _ p) -> TrainState tc 0 0 p)
        -- One pass over training set 
        epochHistory <- forM (P.zip [1..] dataset) $ \(b,chnk) ->
          do
            liftIO $ putStrLn $ "Batch " P.++ (show b)
            (TrainState tc acc loss p) <- S.get
            let (lossAcc,p') = opt p chnk
                (accInc,lossInc) = xtract lossAcc
                (acc',loss') = if (b==1) then (accInc,lossInc) else (acc+accInc, loss+lossInc)
                (scA,scL) = (acc'/(P.fromIntegral b),loss'/(P.fromIntegral b))
            liftIO $ print (scA, scL)
            S.put (TrainState tc acc' loss' p')
            return (scA, scL)
        let epochPrefix val = (show e) P.++ " " P.++ (show val)
            (epochAcc, epochLoss) =  P.last epochHistory
        liftIO $ do hPutStrLn accFile (epochPrefix epochAcc) 
                    hPutStrLn lossFile (epochPrefix epochLoss)
    liftIO $ do hClose accFile
                hClose lossFile

trainMNIST = do
  Just mnistdb <- liftIO $ writeMNIST @128 -- Batch size
  let tc = TrainConfig epochs mnistdb mnistOpt
      epochs = 10
      emptyState = TrainState tc 0 0
  initialWeights <- sample >>= (return . run . AB.map (\x -> (x-0.5)/10) . use)
  hSetBuffering stdout NoBuffering
  evalStateT trainOverBatches (emptyState initialWeights)
  return ()
  where
    mnistOpt = sgd 0.01 (fullyConnectedL (mapB relu) ~> fullyConnectedL @512 softMaxL) crossEntropyL computeAccuracy



computeAccuracy :: forall m n. (KnownNat m, KnownNat n) => L m n -> L m n -> (R 1)
computeAccuracy eval b  = let accInc =   (/ m)
                                          . pack
                                          . A.flatten
                                          . A.fold1 (+)
                                          . A.map (\(A.T2 (A.T2 idx1 _) (A.T2 idx2 _)) -> A.cond ((A.unindex2 idx1) A.== (A.unindex2 idx2)) 1 0)  
                                          . A.fold1 (\(A.T2 idxs1 idxs2) (A.T2 idxs1' idxs2') ->
                                                        let compareIdxs (A.T2 idx x) (A.T2 idx' y) = A.cond (x A.> y) (A.T2 idx x) (A.T2 idx' y)
                                                            newIdx1 = compareIdxs idxs1 idxs1'
                                                            newIdx2 = compareIdxs idxs2 idxs2'
                                                        in (A.T2 newIdx1 newIdx2))
                                          $ A.zip (A.indexed $ unpack eval) (A.indexed $ unpack b) :: R 1
                              m = fromIntegral $ natVal (Proxy @m)
                          in (accInc)
 
--main :: IO ()
-- main =  trainMNIST 
-- trainMNIST = do
--     -- Just test <- liftIO $ loadMNISTB "data/t10k-images-idx3-ubyte" "data/t10k-labels-idx1-ubyte" 
--     -- let testAccuracy w = (\(a,b) -> let len = P.fromIntegral $ P.length test in (a/len,b/len)) $
--     --                        foldl' (\(a,b) (c,d)
--     --                                 -> (a+c,b+d)) (0,0) $
--     --                        (P.map (xtract  .
--     --                                (curry .
--     --                                 run1 $ \(p:::(a:::b)) ->
--     --                                          let eval = evalBP2 learn p a
--     --                                              loss = evalBP2 err eval b
--     --                                              acc = computeAccuracy ((evalBP2 learn) p a) b
--     --                                          in (evalBP2 (AB.++) acc loss)
--     --                                ) w
--     --                               )
--     --                        )
--     --                        test
--     trainOverBatches  
--     --(TrainState _ finalAcc finalLoss _) <- S.get 
--     -- liftIO $ print $ testAccuracy finalWeights
--     -- liftIO $ writeHistory history
--     return ()
