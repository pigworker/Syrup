module Language.Syrup.Fdk.Base where

import Data.Void (Void)

import Language.Syrup.Syn.Base
import Language.Syrup.Doc

------------------------------------------------------------------------------
-- Feedback classes

class Categorise t where
  categorise :: t -> FeedbackStatus

------------------------------------------------------------------------------
-- Scope errors

data ScopeLevel = Local | Global
  deriving (Eq)

levelMsg :: ScopeLevel -> String
levelMsg = \case
  Local  -> "local"
  Global -> "top-level"

data ScopeError
  = OutOfScope ScopeLevel Name Names
    -- TODO?: replace with (l :: ScopeLevel) (VarType l) (Set (VarType l))
    -- name that cannot be resolved & suggestions
  | Shadowing  ScopeLevel Names
    -- TODO?: replace with (l :: ScopeLevel) (Set (VarType l))
    -- shadowing an existing variable

instance Categorise ScopeError where
  categorise = \case
    OutOfScope{}       -> Error
    Shadowing Local _  -> Error
    Shadowing Global _ -> Warning

------------------------------------------------------------------------------
-- Feedback type

data Feedback
  -- internal errors
  = ACouldntFindCircuitDiagram Name
  | AnImpossibleError String

  -- error
  | ACannotDisplayStub Name
  | ANoExecutable String
  | AScopeError ScopeError
  | ASyntaxError Doc
  | ATypeError Doc
  | AnAmbiguousDefinition Name [[String]]
  | AnInvalidTruthTableOutput Name
  | AnUndeclaredCircuit Name
  | AnUndefinedCircuit Name
  | AnUndefinedType TyName
  | AnUnknownIdentifier Name
  | AnIllTypedInputs Name [Ty Unit Void] [Va]
  | AnIllTypedMemory Name [Ty Unit Void] [Va]
  | AnIllTypedOutputs Name [Ty Ti Void] [Va]
  | AWrongFinalMemory [Va] [Va]
  | AWrongOutputSignals [Va] [Va]

  -- warnings
  | AFoundHoles Name [LineDoc] -- non empty list
  | ALint Doc
  | AMissingImplementation Name
  | AStubbedOut Name
  | AnUnreasonablyLargeExperiment Int Int Name

  -- comments
  | ACircuitDefined [Name] -- non empty list
  | ATypeDefined [TyName] -- non empty list

  -- successes
  | ADotGraph [Name] Name [String]
  | ARawCode LineDoc Name Doc
  | ATruthTable Name [String]
  | AnExperiment LineDoc [Name] Doc
  | AnSVGGraph [Name] Name [String]
  | ASuccessfulUnitTest

  -- contextual
  | WhenDisplaying Name [Feedback]
  | WhenUnitTesting Name CircuitConfig CircuitConfig [Feedback]

instance Categorise Feedback where
  categorise = \case
    -- internal errors
    AnImpossibleError{} -> Internal
    ACouldntFindCircuitDiagram{} -> Internal

    -- errors
    ACannotDisplayStub{} -> Error
    ANoExecutable{} -> Error
    AScopeError{} -> Error
    ASyntaxError{} -> Error
    ATypeError{} -> Error
    AnAmbiguousDefinition{} -> Error
    AnInvalidTruthTableOutput{} -> Error
    AnUndeclaredCircuit{} -> Error
    AnUndefinedCircuit{} -> Error
    AnUndefinedType{} -> Error
    AnUnknownIdentifier{} -> Error
    AnIllTypedInputs{} -> Error
    AnIllTypedMemory{} -> Error
    AnIllTypedOutputs{} -> Error
    AWrongFinalMemory{} -> Error
    AWrongOutputSignals{} -> Error

    -- warnings
    AFoundHoles{} -> Warning
    ALint{} -> Warning
    AMissingImplementation{} -> Warning
    AStubbedOut{} -> Warning
    AnUnreasonablyLargeExperiment{} -> Warning

    -- comments
    ACircuitDefined{} -> Comment
    ATypeDefined{} -> Comment

    -- successes
    ADotGraph{} -> Success
    ARawCode{} -> Success
    ATruthTable{} -> Success
    AnExperiment{} -> Success
    AnSVGGraph{} -> Success
    ASuccessfulUnitTest{} -> Success

    -- contextual
    WhenDisplaying _ fdks -> foldMap categorise fdks
    WhenUnitTesting _ _ _ fdks -> foldMap categorise fdks
