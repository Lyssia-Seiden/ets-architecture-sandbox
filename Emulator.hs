module Emulator
  ( ArchState (..),
    Token (..),
    clock,
  )
where

import Data.Bits (Bits (shiftL))
import Data.IntMap qualified as M
import Data.Word

type AWord = Word64

data Port = Left | Right deriving (Show, Enum, Eq)

data Token = Token
  { fp :: Int,
    ip :: Int,
    port :: Port,
    val :: AWord
  }
  deriving (Show, Eq)

data PresenceBits = Empty | Present deriving (Show, Enum)

type Memory = M.IntMap (PresenceBits, AWord)

data ArchState = ArchState
  { mem :: Memory,
    -- I'm using a map here to avoid having to allocate a whole memory space,
    -- but something chunked would be more efficient
    pending :: [Token]
  }
  deriving (Show)

data EffectiveAddressMode = FrameRelative | CodeRelative | Global

data TokenMatchingRule = Dyadic | Monadic | ConstDyadic

data ALUOp = AddVal

data TokenFormingRule = Arith | Switch | Extract | Send

data Dest = Dest Int Port

data Instruction = Instruction
  { ea :: EffectiveAddressMode,
    tm :: TokenMatchingRule,
    ao :: ALUOp,
    tf :: TokenFormingRule,
    d1 :: Dest,
    d2 :: Dest
  }

-- TODO make instructions generic over ISAs
data ISA = ISA
  { parseInstr :: Memory -> Int -> Instruction,
    applyInstr :: Instruction -> Memory -> (Memory, [Token]),
    selector :: [Token] -> ([Token], [Token]) -- (to_exec, rest)
  }

applyToken :: ISA -> Token -> Memory -> (Memory, [Token])
applyToken isa t mem = applyInstr isa (parseInstr isa mem (ip t)) mem

clock :: ISA -> ArchState -> ArchState
clock isa (ArchState mem pending) =
  let -- each cycle we process some subset of the pending tokens
      -- notably what set we process is uarchitectural
      -- so this is making assumptions
      (toProcess, unprocessed) = selector isa pending
      (mem', newTokens) =
        foldl
          (\(m, ts) f -> (fst $ f m, ts ++ snd (f m)))
          (mem, [])
          (map (applyToken isa) toProcess) ::
          (Memory, [Token])
   in ArchState mem' unprocessed