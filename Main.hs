-- {-# LANGUAGE NumericUnderscores #-}

import Data.IntMap qualified as M
import Emulator

-- squallFib calling-convention slots (must match the let bindings inside
-- squallFib). Pulled out so the test harness can construct entry tokens.
entryN, entryRa :: Int
entryN  = 0
entryRa = 1

main :: IO ()
main = do
  -- Heap-indexed ctx allocation; depth limit ~29 (32-bit ctx field).
  -- Test fib(0..15); higher values are correct but slow (naive O(fib(n)) recursion).
  let expected = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 377 + 610]
  mapM_ (\(n, e) -> do
    let r = fibTest n
    putStrLn $ "fib " ++ show n ++ " = " ++ show r
              ++ if r == e then "" else "  *** EXPECTED " ++ show e
    ) (zip [0..] expected)

-- Run the machine until pending is empty or the cycle cap is hit.
runUntilQuiescent :: Int -> ArchState -> ArchState
runUntilQuiescent 0 s = s
runUntilQuiescent n s
  | null (pending s) = s
  | otherwise        = runUntilQuiescent (n - 1) (clock squall s)

initialFibState :: Int -> ArchState
initialFibState n =
  let prog       = zip [0..] squallFib
      raCtx      = 10000
      raStmnt    = 50  -- outside program range; empty mem parses as Dyadic,
                       -- so the single result token writes and doesn't fire
      raTag      = squallPackTag (fromIntegral raCtx) (fromIntegral raStmnt) Emulator.Left
      initialCtx = 8   -- heap index 1 × spacing 8
      memInit    = M.fromList [(a, (Constant, v)) | (a, v) <- prog]
   in ArchState
        { mem = memInit
        , pending =
            [ Token { ctx = initialCtx, stmnt = entryN,  port = Emulator.Left, val = fromIntegral n }
            , Token { ctx = initialCtx, stmnt = entryRa, port = Emulator.Left, val = raTag }
            ]
        }

-- Build initial state for fib(n), clock to quiescence, read result out of the
-- spec-compliant return slot.
fibTest :: Int -> AWord
fibTest n =
  let final = runUntilQuiescent 1000000 (initialFibState n)
      -- The result token lands at addr = raCtx + er(parse(mem[raStmnt])).
      -- mem[raStmnt] is empty -> parses as zero -> er=0 -> addr = raCtx.
   in case mem final M.!? 10000 of
        Just (_, v) -> v
        Nothing     -> error "fibTest: result slot never written"

-- Naive Fibonacci: fib(N) = if N<2 then N else fib(N-1) + fib(N-2)
--
-- Calling convention (2 input tokens per invocation):
--   N  arrives at slot ENTRY_N  (port L)
--   RA arrives at slot ENTRY_RA (port L) -- packed return-address tag
-- Returns by Send-ing one token at the RA tag carrying fib(N).
--
-- See /home/lyssia/.claude/plans/wise-prancing-harp.md for the full design.
squallFib :: [AWord]
squallFib =
  let -- slot indices (positional in the returned list)
      entry_n        = 0
      entry_ra       = 1
      cmp            = 2
      cmp_const      = 3
      switch_addr    = 4
      rec_entry      = 5
      fan_a          = 6
      fan_b          = 7
      fan_c          = 8
      decr1          = 9
      decr1_const    = 10
      decr2          = 11
      decr2_const    = 12
      ctx_extract    = 13
      ctx_shr        = 14
      ctx_shr_const  = 15
      mul_2_33       = 16
      mul_2_33_const = 17
      tag1_dup1      = 18
      tag1_dup2      = 19
      add_2_32       = 20
      add_2_32_const = 21
      tag2_dup       = 22
      add_off_ra_1   = 23
      aor1_const     = 24
      add_off_ra_2   = 25
      aor2_const     = 26
      mint_ret1      = 27
      mint_ret2      = 28
      send_n1        = 29
      send_ra1       = 30
      send_n2        = 31
      send_ra2       = 32
      ret_join       = 33
      final_send     = 34
      -- Frame operand slots (er field) for each Dyadic instruction. 7 slots
      -- (er=64..70), matching the inter-sibling ctx spacing (8). Sends are
      -- self-draining (ack routes back to self; frame slot is Empty after
      -- firing, so the single ack writes harmlessly). Monadic ops use er=76.
      fr_switch  = 64
      fr_final   = 65
      fr_join    = 66
      fr_send_n1 = 67
      fr_send_r1 = 68
      fr_send_n2 = 69
      fr_send_r2 = 70

      -- packed-tag arithmetic constants
      entry_ra_offset = fromIntegral entry_ra * 2 :: AWord -- ENTRY_RA<<1, port L=0
      -- Heap-indexed allocation: child1 = 2*self_ctx, child2 = 2*self_ctx+8.
      -- Spacing=8 >= frame footprint (7). Max depth ~29, supports fib(29).
      sib_mul_const = (2 :: AWord) ^ (33 :: Int)            -- self_ctx → child1<<32
      sib_add_const = (2 :: AWord) ^ (35 :: Int)            -- child1<<32 → child2<<32
      neg_one  = maxBound :: AWord                          -- 2^64 - 1
      neg_two  = maxBound - 1 :: AWord                      -- 2^64 - 2

      -- helper for Monadic ops where d2 is unused (Nop, Dup-with-single-dest,
      -- Extract). The destination is irrelevant but must encode SOMETHING.
      noDest = Dest 0 Emulator.Left

      mon ao' d1' d2' = Instruction
        { ea = FrameRelative, er = 76
        , tm = Monadic, ao = ao', tf = Arith
        , d1 = d1', d2 = d2' }

      -- ConstDyadic Arith with the constant placed at the immediately-next
      -- slot (er=1, ea=CodeRelative). Single output goes to d1'.
      cdyadic ao' d1' = Instruction
        { ea = CodeRelative, er = 1
        , tm = ConstDyadic, ao = ao', tf = Arith
        , d1 = d1', d2 = noDest }

      dyadic er' tf' ao' d1' d2' = Instruction
        { ea = FrameRelative, er = er'
        , tm = Dyadic, ao = ao', tf = tf'
        , d1 = d1', d2 = d2' }

      off here there port = Dest (there - here) port
   in [ -- 0  ENTRY_N: dup N to cmp.L and switch.L
        squallAsm $ mon Dup
          (off entry_n cmp Emulator.Left)
          (off entry_n switch_addr Emulator.Left)

      , -- 1  ENTRY_RA: forward RA to final_send.L (Send uses vl as the tag)
        squallAsm $ mon Nop
          (off entry_ra final_send Emulator.Left)
          noDest

      , -- 2  cmp: ConstDyadic Lt N 2 -> switch.R (predicate)
        squallAsm $ cdyadic Lt
          (off cmp switch_addr Emulator.Right)

      , -- 3  cmp_const: 2
        2

      , -- 4  switch: Dyadic Switch. vl=N, vr=(N<2).
        --     True (N<2):  N -> final_send.R (base case: fib(0)=0, fib(1)=1)
        --     False:       N -> rec_entry.L (recursive case)
        squallAsm $ dyadic fr_switch Switch Nop
          (off switch_addr final_send Emulator.Right)
          (off switch_addr rec_entry Emulator.Left)

      , -- 5  rec_entry: dup N to decr1.L and fan_a (start of trigger chain)
        squallAsm $ mon Dup
          (off rec_entry decr1 Emulator.Left)
          (off rec_entry fan_a Emulator.Left)

      , -- 6  fan_a: dup to decr2.L and fan_b
        squallAsm $ mon Dup
          (off fan_a decr2 Emulator.Left)
          (off fan_a fan_b Emulator.Left)

      , -- 7  fan_b: dup to ctx_extract and fan_c
        squallAsm $ mon Dup
          (off fan_b ctx_extract Emulator.Left)
          (off fan_b fan_c Emulator.Left)

      , -- 8  fan_c: dup to mint_ret1 and mint_ret2 (their Extract triggers)
        squallAsm $ mon Dup
          (off fan_c mint_ret1 Emulator.Left)
          (off fan_c mint_ret2 Emulator.Left)

      , -- 9  decr1: N + (-1) -> send_n1.R (the value-arg for child 1)
        squallAsm $ cdyadic AddVal
          (off decr1 send_n1 Emulator.Right)

      , -- 10 decr1_const: -1
        neg_one

      , -- 11 decr2: N + (-2) -> send_n2.R
        squallAsm $ cdyadic AddVal
          (off decr2 send_n2 Emulator.Right)

      , -- 12 decr2_const: -2
        neg_two

      , -- 13 ctx_extract: Monadic Extract. Mints a tag with our self_ctx in
        --     the high 32 bits, irrelevant slot in low bits. -> ctx_shr.L
        squallAsm $ Instruction
          { ea = FrameRelative, er = 76
          , tm = Monadic, ao = Nop, tf = Extract
          , d1 = off ctx_extract ctx_shr Emulator.Left
          , d2 = noDest -- encoded slot is irrelevant; we Shr the result
          }

      , -- 14 ctx_shr: ConstDyadic Shr 32 -> mul_2_33.L. Drops the low 32 bits
        --     of the minted tag, leaving self_ctx as a value.
        squallAsm $ cdyadic Shr
          (off ctx_shr mul_2_33 Emulator.Left)

      , -- 15 ctx_shr_const: 32
        32

      , -- 16 mul_2_33: ConstDyadic MulVal 2^33 -> tag1_dup1.L
        --     Computes child1_high = (2*self_ctx) << 32. Heap-indexed:
        --     child1_ctx = 2*parent_ctx.
        squallAsm $ cdyadic MulVal
          (off mul_2_33 tag1_dup1 Emulator.Left)

      , -- 17 mul_2_33_const: 2^33 (= 2 << 32)
        sib_mul_const

      , -- 18 tag1_dup1: dup tag1_high to add_2_32 (to compute tag2_high)
        --     and tag1_dup2 (further fan-out for child 1)
        squallAsm $ mon Dup
          (off tag1_dup1 add_2_32 Emulator.Left)
          (off tag1_dup1 tag1_dup2 Emulator.Left)

      , -- 19 tag1_dup2: dup tag1_high to send_n1.L (used as-is for tag1_for_N
        --     since ENTRY_N=0) and add_off_ra_1.L (to add ENTRY_RA offset)
        squallAsm $ mon Dup
          (off tag1_dup2 send_n1 Emulator.Left)
          (off tag1_dup2 add_off_ra_1 Emulator.Left)

      , -- 20 add_2_32: tag1_high + 8*2^32 = tag2_high (child2 = child1 + spacing).
        --     -> tag2_dup
        squallAsm $ cdyadic AddVal
          (off add_2_32 tag2_dup Emulator.Left)

      , -- 21 add_2_32_const: 8 * 2^32 = 2^35
        sib_add_const

      , -- 22 tag2_dup: dup tag2_high to send_n2.L and add_off_ra_2.L
        squallAsm $ mon Dup
          (off tag2_dup send_n2 Emulator.Left)
          (off tag2_dup add_off_ra_2 Emulator.Left)

      , -- 23 add_off_ra_1: tag1_high + (ENTRY_RA<<1) = tag1_for_RA -> send_ra1.L
        squallAsm $ cdyadic AddVal
          (off add_off_ra_1 send_ra1 Emulator.Left)

      , -- 24 aor1_const: ENTRY_RA<<1
        entry_ra_offset

      , -- 25 add_off_ra_2: tag2_high + (ENTRY_RA<<1) = tag2_for_RA -> send_ra2.L
        squallAsm $ cdyadic AddVal
          (off add_off_ra_2 send_ra2 Emulator.Left)

      , -- 26 aor2_const: ENTRY_RA<<1
        entry_ra_offset

      , -- 27 mint_ret1: Monadic Extract. d2 encodes (self_ctx, ret_join, L).
        --     The minted tag (= the return-address we hand to child 1) lands at
        --     send_ra1.R as the value to be Sent.
        squallAsm $ Instruction
          { ea = FrameRelative, er = 76
          , tm = Monadic, ao = Nop, tf = Extract
          , d1 = off mint_ret1 send_ra1 Emulator.Right
          , d2 = off mint_ret1 ret_join Emulator.Left
          }

      , -- 28 mint_ret2: same but encodes (self_ctx, ret_join, R) so child 2's
        --     result lands on the OTHER port of ret_join.
        squallAsm $ Instruction
          { ea = FrameRelative, er = 76
          , tm = Monadic, ao = Nop, tf = Extract
          , d1 = off mint_ret2 send_ra2 Emulator.Right
          , d2 = off mint_ret2 ret_join Emulator.Right
          }

      , -- 29 send_n1: Dyadic Send. L=tag1_for_N, R=N-1. Sends N-1 to child 1's
        --     ENTRY_N. Self-draining: ack routes back here (frame slot is Empty
        --     after firing, ack writes harmlessly).
        squallAsm $ dyadic fr_send_n1 Send Nop
          (Dest 0 Emulator.Left)
          noDest

      , -- 30 send_ra1: L=tag1_for_RA, R=ret-tag-1. Sends RA to child 1's ENTRY_RA.
        squallAsm $ dyadic fr_send_r1 Send Nop
          (Dest 0 Emulator.Left)
          noDest

      , -- 31 send_n2: child 2 N-arg
        squallAsm $ dyadic fr_send_n2 Send Nop
          (Dest 0 Emulator.Left)
          noDest

      , -- 32 send_ra2: child 2 RA
        squallAsm $ dyadic fr_send_r2 Send Nop
          (Dest 0 Emulator.Left)
          noDest

      , -- 33 ret_join: Dyadic AddVal. L=ret1 (from child 1's Send), R=ret2.
        --     -> final_send.R (the recursive case's result)
        squallAsm $ dyadic fr_join Arith AddVal
          (off ret_join final_send Emulator.Right)
          noDest

      , -- 34 final_send: Dyadic Send. L=RA (the destination tag), R=result.
        --     Self-draining.
        squallAsm $ dyadic fr_final Send Nop
          (Dest 0 Emulator.Left)
          noDest
      ]