{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

{-
Note that we imports `ScriptContext` from `PlutusLedgerApi.V2`, which means that
the script created from it will be a PlutusV2 script.
PlutusV2 only supports Plutus Core v1.0.0 (currently the highest and default
version is v1.1.0), which is why the `target-version=1.0.0` flag is needed.
-}

module MSM where

import PlutusTx
import PlutusTx.Prelude qualified as PlutusTx

import PlutusTx.Prelude (
    BuiltinBLS12_381_G1_Element,
    bls12_381_G1_add,
    bls12_381_G1_compress,
    bls12_381_G1_compressed_generator,
    bls12_381_G1_compressed_zero,
    bls12_381_G1_scalarMul,
    bls12_381_G1_uncompress,
 )

-- calculate MSM using bls12_381_G1_scalarMul and bls12_381_G1_add
{-# INLINEABLE calculateMSM #-}
calculateMSM :: [Integer] -> [BuiltinBLS12_381_G1_Element] -> BuiltinBLS12_381_G1_Element
calculateMSM scalars points =
    let
        zipped = (PlutusTx.zip scalars points)
        mapped = PlutusTx.map (\(a, g) -> bls12_381_G1_scalarMul a g) zipped
     in
        foldl (\a b -> bls12_381_G1_add a b) (bls12_381_G1_uncompress bls12_381_G1_compressed_zero) mapped

wrapMSM :: [Integer] -> [PlutusTx.BuiltinByteString] -> PlutusTx.BuiltinByteString
wrapMSM s p = bls12_381_G1_compress (calculateMSM s (PlutusTx.map (\e -> bls12_381_G1_uncompress e) p))

appliedCompiled :: [Integer] -> [BuiltinBLS12_381_G1_Element] -> CompiledCode PlutusTx.BuiltinByteString
appliedCompiled scalars points =
    $$(PlutusTx.compile [||wrapMSM||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCodeDef scalars
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCodeDef (PlutusTx.map (\e -> bls12_381_G1_compress e) points)
