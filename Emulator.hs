module Emulator
  ( ArchState (..),
    Token (..),
    Port (..),
    squall,
    PresenceBits (..),
    clock,
    AWord,
    squallAsm,
    Instruction (..),
    EffectiveAddressMode (..),
    TokenMatchingRule (..),
    ALUOp (..),
    TokenFormingRule (..),
    Dest (..),
    trace',
  )
where

import Data.Bits (Bits (shiftL, shiftR, (.&.)), (.|.))
import Data.IntMap qualified as M
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe)
import Data.Word
import Debug.Trace (trace)

type AWord = Word64

data Port = Left | Right deriving (Show, Enum, Eq)

portToInt :: Port -> AWord
portToInt Emulator.Left = 0
portToInt Emulator.Right = 1

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

data EffectiveAddressMode = FrameRelative | CodeRelative | Global deriving (Show, Eq, Enum)

data TokenMatchingRule = Dyadic | Monadic | ConstDyadic deriving (Show, Eq, Enum)

data ALUOp = Nop | AddVal | Lt | Dup | MulVal deriving (Show, Eq, Enum)

data TokenFormingRule = Arith | Switch | Extract | Send deriving (Show, Eq, Enum)

data Dest = Dest {offset :: Int, p :: Port} deriving (Show, Eq)

data Instruction = Instruction
  { ea :: EffectiveAddressMode,
    er :: Int,
    tm :: TokenMatchingRule,
    ao :: ALUOp,
    tf :: TokenFormingRule,
    d1 :: Dest,
    d2 :: Dest
  }
  deriving (Show, Eq)

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
          (\(m, ts) f -> (fst $ f m, ts ++ snd (f m)))
          (mem, [])
          (map (applyToken isa) toProcess) ::
          (Memory, [Token])
   in ArchState mem' (unprocessed ++ newTokens)

squall :: ISA
squall =
  ISA
    { parseInstr = squallParse,
      applyInstr = squallApply,
      selector = squallSelector
    }

squallSelector :: [Token] -> ([Token], [Token])
squallSelector (t : ts) = ([t], ts)
squallSelector [] = ([], [])

squallEARepr :: [(AWord, EffectiveAddressMode)]
squallEARepr =
  [ (0b00, FrameRelative),
    (0b01, CodeRelative),
    (0b10, Global)
  ]

squallParseEA :: AWord -> EffectiveAddressMode
squallParseEA a = snd . fromJust $ find ((== a) . fst) squallEARepr

squallAsmEA :: EffectiveAddressMode -> AWord
squallAsmEA ea = fst . fromJust $ find ((== ea) . snd) squallEARepr

squallTMRepr :: [(AWord, TokenMatchingRule)]
squallTMRepr =
  [ (0b00, Dyadic),
    (0b01, Monadic),
    (0b10, ConstDyadic)
  ]

squallParseTM :: AWord -> TokenMatchingRule
squallParseTM a = snd . fromJust $ find ((== a) . fst) squallTMRepr

squallAsmTM :: TokenMatchingRule -> AWord
squallAsmTM a = fst . fromJust $ find ((== a) . snd) squallTMRepr

squallAORepr :: [(AWord, ALUOp)]
squallAORepr =
  [ (0b00000, Nop),
    (0b00001, AddVal),
    (0b00010, Lt),
    (0b00011, Dup),
    (0b00100, MulVal)
  ]

squallParseAO :: AWord -> ALUOp
squallParseAO a = snd . fromJust $ find ((== a) . fst) squallAORepr

squallAsmAO :: ALUOp -> AWord
squallAsmAO a = fst . fromJust $ find ((== a) . snd) squallAORepr

squallTFRepr :: [(AWord, TokenFormingRule)]
squallTFRepr =
  [ (0b00, Arith),
    (0b01, Switch),
    (0b10, Send),
    (0b11, Extract)
  ]

squallParseTF :: AWord -> TokenFormingRule
squallParseTF a = snd . fromJust $ find ((== a) . fst) squallTFRepr

squallAsmTF :: TokenFormingRule -> AWord
squallAsmTF a = fst . fromJust $ find ((== a) . snd) squallTFRepr

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
-- TODO is this correct
matchingFunction Dyadic port Constant = (Constant, Read, True)
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
squallALUOp Nop lhs _ = lhs
squallALUOp AddVal lhs rhs = lhs + rhs
squallALUOp Lt lhs rhs = if lhs < rhs then 1 else 0
squallALUOp Dup lhs _ = lhs
squallALUOp MulVal lhs rhs = lhs * rhs

squallALUOutputLR :: ALUOp -> (Bool, Bool)
squallALUOutputLR Nop = (True, False)
squallALUOutputLR AddVal = (True, False)
squallALUOutputLR Lt = (True, False)
squallALUOutputLR Dup = (True, True)
squallALUOutputLR MulVal = (True, False)

squallParseTag :: AWord -> (AWord, AWord, Port)
squallParseTag w =
  ( w `shiftR` 32,
    (w `shiftR` 1) .&. (2 ^ 32 - 1),
    if even w then Emulator.Right else Emulator.Left
  )

squallPackTag :: AWord -> AWord -> Port -> AWord
squallPackTag ctx stmnt port =
  (ctx `shiftL` 32)
    .|. ((stmnt `shiftL` 1) .&. (2 ^ 32 - 1))
    .|. if port == Emulator.Left then 0 else 1

squallApply :: Token -> Instruction -> Memory -> (Memory, [Token])
squallApply t instr mem =
  let addr = case ea instr of
        FrameRelative ->
          ctx t
            + er
              ( squallParse mem $
                  -- isnt this just instr?
                  fromIntegral $
                    snd (mem `memRead` stmnt t)
              )
        CodeRelative -> stmnt t + er instr
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
        Switch ->
          let s' = stmnt t + offset (d1 instr)
              s'' = stmnt t + offset (d2 instr)
              (il, ir) = (vr /= 0, vr == 0)
           in ( [ Token
                    { ctx = ctx t,
                      stmnt = s',
                      port = p $ d1 instr,
                      val = vl
                    }
                  | il && issue
                ]
              )
                ++ ( [ Token
                         { ctx = ctx t,
                           stmnt = s'',
                           port = p $ d2 instr,
                           val = vl
                         }
                       | ir && issue
                     ]
                   )
        Send ->
          let (ctx', stmnt', port') = squallParseTag vl

              s' = stmnt t + offset (d1 instr)
           in [ Token
                  { ctx = fromIntegral ctx',
                    stmnt = fromIntegral stmnt',
                    port = port',
                    val = vr
                  },
                Token
                  { ctx = ctx t,
                    stmnt = s',
                    port = p $ d1 instr,
                    val = vr
                  }
              ]
        Extract ->
          let s' = stmnt t + offset (d1 instr)
              s'' = stmnt t + offset (d2 instr)
           in [ Token
                  { ctx = ctx t,
                    stmnt = s',
                    port = p $ d1 instr,
                    val =
                      squallPackTag
                        (fromIntegral $ ctx t)
                        (fromIntegral s'')
                        (p $ d2 instr)
                  }
              ]
   in (mem', generatedTokens)

squallAsm :: Instruction -> AWord
squallAsm instr =
  let place w o v = (v .&. ((2 ^ (w + 1)) - 1)) `shiftL` o
   in place 2 0 (squallAsmTF $ tf instr)
        .|. place 6 2 (squallAsmAO $ ao instr)
        .|. place 2 8 (squallAsmTM $ tm instr)
        .|. place 2 10 (squallAsmEA $ ea instr)
        .|. place 10 12 (fromIntegral $ er instr)
        .|. place 1 22 (portToInt $ p $ d1 instr)
        .|. place 20 23 (fromIntegral $ offset $ d1 instr)
        .|. place 1 43 (portToInt $ p $ d2 instr)
        .|. place 20 44 (fromIntegral $ offset $ d2 instr)
