{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Data.Text as T
import GHC.TypeError (ErrorMessage(T.Text))

data OpCode
  = ThreeAddrCodeOp
  | Load
  | Store
  | Const
  deriving (Show)

newtype Op = OpCode [Int]
  deriving (Show)

data SuperInstOperads = OperandInt Int | OperandOp Op
  deriving (Show)

data SuperInst
  = LoadConstOp
  | LoadConstOpStore
  | LoadLoadOp
  | LoadLoadOpStore
  | ConstLoadOp
  | ConstLoadOpStore
  | ConstConstOp
  | ConstConstOpStore
  | JmpIfNotCond

-- Rule should represent that
-- if field `i` of Tuple l == C1
-- and field `j` of the Tuple m == C2 then
-- then check that Tuple l's `x` field == Tuple m's `y` field
-- constant is always an OpCode
data OpMatchRule
  = OpMatch
  { indexOfInst :: Int,
    opCode :: OpCode
  }

data FieldMatchRule = FieldMatch
  { indexOfInst1 :: Int,
    indexOfInst2 :: Int,
    fieldIndexInInst1 :: Int,
    fieldIndexInInst2 :: Int
  }

data TranslationRule = Translation
  { -- what fields to use in the superinst
    fields :: [(Int, Int)], -- (index of inst, field index)
    superInst :: SuperInst
  }

type Rule = ([OpMatchRule], [FieldMatchRule], [TranslationRule])

--  rules for finding longest sequence of instructions which can be converted to a single instruction
--  1. Load v1 r1, Const v2 r2, OpCode r1 r2 r3 => LoadConstOp v1 v2 r3 OpCode
--  2. Load v1 r1, Load v2 r2, OpCode r1 r2 r3 => LoadLoadOp v1 v2 r3 OpCode
--  3. Const v1 r1, Load v2 r2, OpCode r1 r2 r3 => ConstLoadOp v1 v2 r3 OpCode
--  4. Const v1 r1, Const v2 r2, OpCode r1 r2 r3 => ConstConstOp v1 v2 r3 OpCode

genLoadConstRulesPermutations :: [Rule]
genLoadConstRulesPermutations =
  let ops = [Load, Const]
   in [gen op1 op2 (getSupop op1 op2) | op1 <- ops, op2 <- ops]
  where
    gen op1 op2 supop =
      ( [ OpMatch {indexOfInst = 0, opCode = op1},
          OpMatch {indexOfInst = 1, opCode = op2}
        ],
        [ FieldMatch {indexOfInst1 = 0, indexOfInst2 = 2, fieldIndexInInst1 = 1, fieldIndexInInst2 = 0},
          FieldMatch {indexOfInst1 = 1, indexOfInst2 = 2, fieldIndexInInst1 = 1, fieldIndexInInst2 = 1}
        ],
        [ Translation {fields = [(0, 0), (1, 0), (2, 1)], superInst = supop}
        ]
      )
    getSupop Load Load = LoadLoadOp
    getSupop Const Load = ConstLoadOp
    getSupop Load Const = ConstLoadOp
    getSupop Const Const = ConstConstOp

rules :: [Rule]
rules = genLoadConstRulesPermutations

-- generate rust code for the lr parser table
genTable :: [Rule] -> T.Text
genTable rules = T.concat ["fn get_rules() -> Vec<Rule> {\n", T.intercalate ",\n" (map genRule rules), "\n}\n"]
  where
    genRule (opMatchRules, fieldMatchRules, translationRules) =
      T.concat
        [ "Rule {",
          "op_match_rules: vec![",
          T.intercalate ", " (map genOpMatchRule opMatchRules),
          "],",
          "field_match_rules: vec![",
          T.intercalate ", " (map genFieldMatchRule fieldMatchRules),
          "],",
          "translation_rules: vec![",
          T.intercalate ", " (map genTranslationRule translationRules),
          "]",
          "}"
        ]
    genOpMatchRule (OpMatch i op) = T.concat ["OpMatch {index_of_inst: ", T.pack (show i), ", op_code: ", T.pack (show op), "}"]
    genFieldMatchRule (FieldMatch i1 i2 f1 f2) = T.concat ["FieldMatch {index_of_inst1: ", T.pack (show i1), ", index_of_inst2: ", T.pack (show i2), ", field_index_in_inst1: ", T.pack (show f1), ", field_index_in_inst2: ", T.pack (show f2), "}"]
    genTranslationRule (Translation fields superInst) = T.concat ["Translation {fields: vec![", T.intercalate ", " (map genField fields), "], super_inst: ", T.pack (show superInst), "}"]
    genField (i, f) = T.concat ["(", T.pack (show i), ", ", T.pack (show f), ")"]

main :: IO ()
main = putStrLn "Hello, Haskell!"
