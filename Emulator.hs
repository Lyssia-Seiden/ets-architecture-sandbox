module Emulator
  ( ArchState (..),
    Token (..),
    Port (..),
    squall,
    PresenceBits (..),
    clock,
  )
where

import Data.Bits (Bits (shiftL, shiftR, (.&.)))
import Data.IntMap qualified as M
import Data.Maybe (fromMaybe)
import Data.Word
import Debug.Trace (trace)

type AWord = Word64

data Port = Left | Right deriving (Show, Enum, Eq)

data Token = Token
  { ctx :: Int,
    stmnt :: Int,
    port :: Port,
    val :: AWord
  }
  deriving (Show, Eq)

data PresenceBits = Empty | Present | Constant deriving (Show, Enum)

type Memory = M.IntMap (PresenceBits, AWord)

memRead :: Memory -> Int -> (PresenceBits, AWord)
memRead m a = fromMaybe (Empty, 0) (m M.!? a)

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

data Dest = Dest {offset :: Int, p :: Port}

data Instruction = Instruction
  { ea :: EffectiveAddressMode,
    er :: Int,
    tm :: TokenMatchingRule,
    ao :: ALUOp,
    tf :: TokenFormingRule,
    d1 :: Dest,
    d2 :: Dest
  }

-- TODO make instructions generic over ISAs
data ISA = ISA
  { parseInstr :: Memory -> Int -> Instruction,
    applyInstr :: Token -> Instruction -> Memory -> (Memory, [Token]),
    selector :: [Token] -> ([Token], [Token]) -- (to_exec, rest)
  }

applyToken :: ISA -> Token -> Memory -> (Memory, [Token])
applyToken isa t mem = applyInstr isa t (parseInstr isa mem (stmnt t)) mem

trace' :: (Show a) => a -> a
trace' v = trace (show v) v

clock :: ISA -> ArchState -> ArchState
clock isa (ArchState mem pending) =
  let -- each cycle we process some subset of the pending tokens
      -- notably what set we process is uarchitectural
      -- so this is making assumptions
      (toProcess, unprocessed) = selector isa pending
      (mem', newTokens) =
        foldl
          (\(m, ts) f -> trace' (fst $ f m, ts ++ snd (f m)))
          (mem, [])
          (map (applyToken isa) toProcess) ::
          (Memory, [Token])
   in ArchState mem' (unprocessed ++ newTokens)

squall :: ISA
squall =
  ISA
    { parseInstr = squallParse,
      applyInstr = squallApply,
      selector = (,[])
    }

squallParseEA :: AWord -> EffectiveAddressMode
squallParseEA 0b00 = FrameRelative
squallParseEA 0b01 = CodeRelative
squallParseEA 0b10 = Global

squallParseTM :: AWord -> TokenMatchingRule
squallParseTM 0b00 = Dyadic
squallParseTM 0b01 = Monadic
squallParseTM 0b10 = ConstDyadic

squallParseAO :: AWord -> ALUOp
squallParseAO 0b00000 = AddVal

squallParseTF :: AWord -> TokenFormingRule
squallParseTF 0b00 = Arith
squallParseTF 0b01 = Switch
squallParseTF 0b10 = Send
squallParseTF 0b11 = Extract

squallParse :: Memory -> Int -> Instruction
squallParse mem addr =
  let (_, val) = mem `memRead` addr
      er = (val `shiftR` 12) .&. 0x3FF
      ea = (val `shiftR` 10) .&. 0b11
      tm = (val `shiftR` 8) .&. 0b11
      ao = (val `shiftR` 2) .&. 0x1F
      tf = val .&. 0b11

      d1 =
        Dest
          ((fromIntegral val `shiftR` 23) .&. 0xFFFFF)
          ( if ((val `shiftR` 22) .&. 0b1) == 1
              then Emulator.Right
              else Emulator.Left
          )
      d2 =
        Dest
          ((fromIntegral val `shiftR` 44) .&. 0xFFFFF)
          ( if ((val `shiftR` 43) .&. 0b1) == 1
              then Emulator.Right
              else Emulator.Left
          )
   in Instruction
        { ea = squallParseEA ea,
          er = fromIntegral er,
          tm = squallParseTM tm,
          ao = squallParseAO ao,
          tf = squallParseTF tf,
          d1 = d1,
          d2 = d2
        }

data MemOp = Read | Write | Exchange | DecrExchange

matchingFunction :: TokenMatchingRule -> Port -> PresenceBits -> (PresenceBits, MemOp, Bool)
matchingFunction Dyadic port Empty = (Present, Write, False)
matchingFunction Dyadic port Present = (Empty, Read, True)
matchingFunction Monadic Emulator.Left pres = (pres, Read, True)
matchingFunction ConstDyadic Emulator.Left Empty =
  (Present, Write, False)
matchingFunction ConstDyadic Emulator.Right Empty =
  (Constant, Write, False)
matchingFunction ConstDyadic Emulator.Right Present =
  (Constant, Exchange, True)
matchingFunction ConstDyadic Emulator.Left Constant =
  (Constant, Read, True)

squallALUOp :: ALUOp -> AWord -> AWord -> AWord
squallALUOp AddVal = (+)

squallALUOutputLR :: ALUOp -> (Bool, Bool)
squallALUOutputLR AddVal = (True, False)

squallApply :: Token -> Instruction -> Memory -> (Memory, [Token])
squallApply t instr mem =
  let addr = case ea instr of
        FrameRelative ->
          ctx t
            + er
              ( squallParse mem $
                  fromIntegral $
                    snd (mem `memRead` stmnt t)
              )
        CodeRelative -> ctx t + er instr
        Global -> er instr
      (pb, memVal) = mem `memRead` fromIntegral addr
      (pb', mop, issue) = matchingFunction (tm instr) (port t) pb
      memVal' = case mop of
        Read -> memVal
        Write -> Emulator.val t
        Exchange ->
          Emulator.val t
        DecrExchange -> error "todo"
      mem' = M.insert addr (pb', memVal') mem

      (vl, vr) = case tm instr of
        Dyadic ->
          if port t == Emulator.Left
            then (val t, memVal)
            else (memVal, val t)
        Monadic -> (val t, val t)
        ConstDyadic -> error "todo" -- handle sorting const/val
      generatedTokens = case tf instr of
        Arith ->
          let v' = squallALUOp (ao instr) vl vr
              s' = stmnt t + offset (d1 instr)
              s'' = stmnt t + offset (d2 instr)
              (il, ir) = squallALUOutputLR (ao instr)
           in ( [ Token
                    { ctx = ctx t,
                      stmnt = s',
                      port = p $ d1 instr,
                      val = v'
                    }
                  | il && issue
                ]
              )
                ++ ( [ Token
                         { ctx = ctx t,
                           stmnt = s'',
                           port = p $ d2 instr,
                           val = v'
                         }
                       | ir && issue
                     ]
                   )
        Switch -> error "todo"
        Send -> error "todo"
        Extract -> error "todo"
   in (mem', generatedTokens)