module Assembler
  ( Node (..),
    NodeBody (..),
    Ref (..),
    assemble,
  )
where

import Data.Map.Strict qualified as Map
import Emulator
  ( AWord,
    ALUOp (..),
    Dest (..),
    EffectiveAddressMode (..),
    Instruction (..),
    Port (..),
    TokenFormingRule (..),
    TokenMatchingRule (..),
    squallAsm,
  )
import Emulator qualified

-- | A reference to a named node's input port.
data Ref = Ref String Port deriving (Show, Eq)

-- | A high-level dataflow node.
data Node = Node String NodeBody deriving (Show)

-- | The body of a dataflow node.
data NodeBody
  = Mono ALUOp [Ref]
  | CDyadic ALUOp AWord [Ref]
  | DyadicA ALUOp [Ref]
  | Sw [Ref] [Ref]
  | Snd
  | Ext [Ref] Ref
  deriving (Show)

------------------------------------------------------------
-- Internal types
------------------------------------------------------------

data LDest = LRef String Port | LNoDest | LSelf Port
  deriving (Show)

data LoInstr
  = LoMono    String ALUOp LDest LDest
  | LoCDyad   String ALUOp AWord LDest
  | LoDyad    String TokenFormingRule ALUOp LDest LDest
  | LoExtract String LDest LDest
  deriving (Show)

loName :: LoInstr -> String
loName (LoMono n _ _ _)    = n
loName (LoCDyad n _ _ _)   = n
loName (LoDyad n _ _ _ _)  = n
loName (LoExtract n _ _)   = n

loWidth :: LoInstr -> Int
loWidth (LoCDyad {}) = 2
loWidth _            = 1

------------------------------------------------------------
-- Step 1: Expand multi-destination nodes into Dup chains
------------------------------------------------------------

expandDups :: [Node] -> [LoInstr]
expandDups = concatMap expandNode

expandNode :: Node -> [LoInstr]
expandNode (Node name body) = case body of
  Mono ao refs       -> expandMono name ao refs
  CDyadic ao c refs  -> expandCDyadic name ao c refs
  DyadicA ao refs    -> expandDyadicA name ao refs
  Sw ts fs           -> expandSwitch name ts fs
  Snd                -> [LoDyad name Send Nop (LSelf Emulator.Left) LNoDest]
  Ext d1s encRef     -> expandExtract name d1s encRef

expandMono :: String -> ALUOp -> [Ref] -> [LoInstr]
expandMono name ao refs = case (ao, refs) of
  (Nop, [])     -> [LoMono name Nop LNoDest LNoDest]
  (Nop, [a])    -> [LoMono name Nop (rl a) LNoDest]
  (Nop, [a, b]) -> [LoMono name Dup (rl a) (rl b)]
  (Nop, a:rest) -> let (d2, chain) = fanOut name rest
                   in LoMono name Dup (rl a) d2 : chain
  (_, [])       -> [LoMono name ao LNoDest LNoDest]
  (_, [a])      -> [LoMono name ao (rl a) LNoDest]
  (_, _)        -> let (d1, chain) = fanOut name refs
                   in LoMono name ao d1 LNoDest : chain

expandCDyadic :: String -> ALUOp -> AWord -> [Ref] -> [LoInstr]
expandCDyadic name ao c refs = case refs of
  []  -> [LoCDyad name ao c LNoDest]
  [a] -> [LoCDyad name ao c (rl a)]
  _   -> let (d1, chain) = fanOut name refs
         in LoCDyad name ao c d1 : chain

expandDyadicA :: String -> ALUOp -> [Ref] -> [LoInstr]
expandDyadicA name ao refs = case refs of
  []  -> [LoDyad name Arith ao LNoDest LNoDest]
  [a] -> [LoDyad name Arith ao (rl a) LNoDest]
  _   -> let (d1, chain) = fanOut name refs
         in LoDyad name Arith ao d1 LNoDest : chain

expandSwitch :: String -> [Ref] -> [Ref] -> [LoInstr]
expandSwitch name trueRefs falseRefs =
  let (d1, tc) = fanOutN (name ++ "~t") trueRefs
      (d2, fc) = fanOutN (name ++ "~f") falseRefs
  in LoDyad name Switch Nop d1 d2 : tc ++ fc

expandExtract :: String -> [Ref] -> Ref -> [LoInstr]
expandExtract name d1Refs (Ref en ep) = case d1Refs of
  []  -> [LoExtract name LNoDest (LRef en ep)]
  [a] -> [LoExtract name (rl a) (LRef en ep)]
  _   -> let (d1, chain) = fanOut name d1Refs
         in LoExtract name d1 (LRef en ep) : chain

fanOut :: String -> [Ref] -> (LDest, [LoInstr])
fanOut name = fanOutN (name ++ "~fan")

fanOutN :: String -> [Ref] -> (LDest, [LoInstr])
fanOutN _ []  = (LNoDest, [])
fanOutN _ [a] = (rl a, [])
fanOutN base refs = (LRef (label 0) Emulator.Left, go 0 refs)
  where
    label i = if i == 0 then base else base ++ show i
    go i [a, b]    = [LoMono (label i) Dup (rl a) (rl b)]
    go i (a:rest)  = LoMono (label i) Dup (rl a) (LRef (label (i+1)) Emulator.Left)
                     : go (i+1) rest
    go _ []        = []

rl :: Ref -> LDest
rl (Ref n p) = LRef n p

------------------------------------------------------------
-- Step 2: Assign slot indices
------------------------------------------------------------

type SlotMap = Map.Map String Int

assignSlots :: [LoInstr] -> (SlotMap, [(Int, LoInstr)])
assignSlots = go 0 Map.empty
  where
    go _ m [] = (m, [])
    go slot m (instr:rest) =
      let n = loName instr
          m' = Map.insert n slot m
          (m'', items) = go (slot + loWidth instr) m' rest
      in (m'', (slot, instr) : items)

------------------------------------------------------------
-- Step 3: Resolve labels, assign er, emit
------------------------------------------------------------

erBase :: Int
erBase = 64

assemble :: [Node] -> (Map.Map String Int, [AWord])
assemble nodes =
  let expanded = expandDups nodes
      (slotMap, items) = assignSlots expanded
      dyadicNames = [loName i | (_, i@(LoDyad {})) <- items]
      erMap = Map.fromList $ zip dyadicNames [erBase..]
  in (slotMap, concatMap (emitInstr slotMap erMap) items)

emitInstr :: SlotMap -> Map.Map String Int -> (Int, LoInstr) -> [AWord]
emitInstr sm em (slot, instr) = case instr of
  LoMono _ ao d1 d2 ->
    [ squallAsm Instruction
        { ea = FrameRelative, er = 0
        , tm = Monadic, ao = ao, tf = Arith
        , d1 = res d1, d2 = res d2 } ]
  LoCDyad _ ao c d1 ->
    [ squallAsm Instruction
        { ea = CodeRelative, er = 1
        , tm = ConstDyadic, ao = ao, tf = Arith
        , d1 = res d1, d2 = noDst }
    , c ]
  LoDyad name tf ao d1 d2 ->
    [ squallAsm Instruction
        { ea = FrameRelative, er = em Map.! name
        , tm = Dyadic, ao = ao, tf = tf
        , d1 = res d1, d2 = res d2 } ]
  LoExtract _ d1 d2 ->
    [ squallAsm Instruction
        { ea = FrameRelative, er = 0
        , tm = Monadic, ao = Nop, tf = Extract
        , d1 = res d1, d2 = res d2 } ]
  where
    res (LRef name port) = case Map.lookup name sm of
      Just target -> Dest (target - slot) port
      Nothing     -> error $ "assemble: unknown label '" ++ name ++ "'"
    res LNoDest      = noDst
    res (LSelf port) = Dest 0 port
    noDst = Dest 0 Emulator.Left
