{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens (Bifunctor (bimap), traverseOf, (^.))
import Control.Monad.Except (runExceptT)
import Data.ByteString
import Flat (flat)
import MSM
import PlutusCore qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import UntypedPlutusCore (UnrestrictedProgram (UnrestrictedProgram))

import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as PLC
import PlutusTx (
    CompiledCode,
    CompiledCodeIn,
    getPlcNoAnn,
 )
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as UPLC

import System.Random (mkStdGen, randomRs)

import PlutusTx.Prelude (
    BuiltinBLS12_381_G1_Element,
    bls12_381_G1_compressed_generator,
    bls12_381_G1_scalarMul,
    bls12_381_G1_uncompress,
 )

-- import Prelude (pure)

data Error
    = FreeVariableError
    | EvaluationError (UPLC.CekEvaluationException UPLC.Name UPLC.DefaultUni UPLC.DefaultFun) ExBudget
    deriving (Show)

evalWithBudget :: CompiledCode a -> Either Main.Error ExBudget
evalWithBudget compiledCode =
    let programE =
            PLC.runQuote $
                runExceptT @PLC.FreeVariableError $
                    traverseOf UPLC.progTerm UPLC.unDeBruijnTerm $
                        getPlcNoAnn compiledCode
     in case programE of
            Left _ -> Left FreeVariableError
            Right program ->
                let (result, UPLC.TallyingSt _ budget) =
                        UPLC.runCekNoEmit
                            PLC.defaultCekParametersForTesting
                            UPLC.tallying
                            $ program ^. UPLC.progTerm
                 in bimap (`EvaluationError` budget) (const budget) result

-- this dumps contract to file to get profiling data
-- writeToFile :: CompiledCodeIn -> Integer -> IO ()
writeToFile p tcid =
    Data.ByteString.writeFile ((show tcid) ++ "MSM.flat") . flat . UnrestrictedProgram <$> PlutusTx.getPlcNoAnn $
        p

bls12_381_order :: Integer
bls12_381_order = 52435875175126190479447740508185965837690552500527637822603658699938581184513

generateInputs :: Int -> ([Integer], [BuiltinBLS12_381_G1_Element])
generateInputs n =
    let
        g = bls12_381_G1_uncompress bls12_381_G1_compressed_generator
        seed = 42
        pureGen = mkStdGen seed
        randomPoints = Prelude.map (\e -> bls12_381_G1_scalarMul e g) (Prelude.take n (randomRs (1, bls12_381_order) pureGen))
        randomScalars :: [Integer]
        randomScalars = (Prelude.take n (randomRs (1, bls12_381_order) pureGen))
     in
        (randomScalars, randomPoints)

main :: IO ()
main = do
    let testCases = [10, 30, 50, 80, 100, 300, 500, 800, 1000]
    let inputData = Prelude.map (\e -> generateInputs e) testCases
    let functions = Prelude.map (\(s, g) -> appliedCompiled s g) inputData
    let executed =
            Prelude.map
                ( \(p, tcid) -> do
                    writeToFile p tcid
                    case evalWithBudget p of
                        Left e ->
                            putStrLn $ "Fail: " <> show e
                        Right budget ->
                            putStrLn $ "Resources used: " <> show budget
                )
                (Prelude.zip functions testCases)
    Prelude.foldl (>>) (Prelude.pure ()) executed
