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
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

{-
Note that we imports `ScriptContext` from `PlutusLedgerApi.V2`, which means that
the script created from it will be a PlutusV2 script.
PlutusV2 only supports Plutus Core v1.0.0 (currently the highest and default
version is v1.1.0), which is why the `target-version=1.0.0` flag is needed.
-}

module MSM where

import PlutusCore.Version (plcVersion100)
import PlutusTx
import PlutusTx.Prelude qualified as PlutusTx

import PlutusTx.Prelude (
    BuiltinBLS12_381_G1_Element,
    bls12_381_G1_add,
    bls12_381_G1_compressed_zero,
    bls12_381_G1_scalarMul,
    bls12_381_G1_uncompress,
    emptyByteString,
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

decompressed :: BuiltinBLS12_381_G1_Element
decompressed = (bls12_381_G1_uncompress bls12_381_G1_compressed_zero)

adapter :: BuiltinData -> ()
adapter _ =
    let result = calculateMSM [1, 2, 3] [decompressed, decompressed, decompressed]
     in if result PlutusTx.== decompressed
            then ()
            else PlutusTx.error ()

compiled :: CompiledCode (BuiltinData -> ())
compiled = $$(PlutusTx.compile [||adapter||])

inputDataCompiled :: CompiledCode BuiltinData
inputDataCompiled =
    let proof = PlutusTx.toBuiltinData emptyByteString
     in proof `seq` PlutusTx.liftCode plcVersion100 proof

appliedCompiled :: CompiledCode ()
appliedCompiled =
    case compiled `applyCode` inputDataCompiled of
        Left e -> error $ show e
        Right applied -> applied
