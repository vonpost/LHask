{ mkDerivation, accelerate, accelerate-blas
, accelerate-io-bytestring, accelerate-llvm, accelerate-llvm-native
, accelerate-llvm-ptx, array, backprop, base, binary, deepseq
, directory, lens, lens-accelerate, mnist-idx
, mwc-random-accelerate, random, stdenv, transformers, vector
}:
mkDerivation {
  pname = "HLearn";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [
    accelerate accelerate-blas accelerate-io-bytestring accelerate-llvm
    accelerate-llvm-native accelerate-llvm-ptx array backprop base
    binary deepseq directory lens lens-accelerate mnist-idx
    mwc-random-accelerate random transformers vector
  ];
  doHaddock = false;
  license = stdenv.lib.licenses.bsd3;
}
