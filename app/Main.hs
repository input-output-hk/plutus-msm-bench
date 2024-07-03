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
    getPlcNoAnn,
 )
import PlutusTx.Prelude (BuiltinBLS12_381_G1_Element)
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as UPLC

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
writeToFile :: IO ()
writeToFile =
    Data.ByteString.writeFile "MSM.flat" . flat . UnrestrictedProgram <$> PlutusTx.getPlcNoAnn $
        appliedCompiled

main :: IO ()
main = do
    writeToFile
    case evalWithBudget appliedCompiled of
        Left e ->
            putStrLn $ "Fail: " <> show e
        Right budget ->
            putStrLn $ "Resources used: " <> show budget
