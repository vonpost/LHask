# LHask
To train MNIST with GPU change back-end in Backprop.hs to Data.Array.Accelerate.LLVM.PTX
To train MNIST with CPU change back-end in Backprop.hs to Data.Array.Accelerate.LLVM.Native

A directory named "data" must be located in the same directory as the resulting executable after compilation. It must contain the MNIST database as .idx. 
You can download MNIST from http://yann.lecun.com/exdb/mnist/

When compiling for the GPU you must pass the additional flag -threaded to GHC
