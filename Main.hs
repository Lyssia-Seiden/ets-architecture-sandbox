-- {-# LANGUAGE NumericUnderscores #-}

import Data.IntMap qualified as M
import Emulator

main =
  let instrs =
        [ ( 0,
            squallAsm $
              Emulator.trace'
                Emulator.Instruction
                  { ea = FrameRelative,
                    er = 1,
                    tm = Dyadic,
                    ao = AddVal,
                    tf = Arith,
                    d1 = Dest 1 Emulator.Left,
                    d2 = Dest 0 Emulator.Left
                  }
          ),
          ( 1,
            squallAsm $
              Emulator.trace'
                Emulator.Instruction
                  { ea = FrameRelative,
                    er = 0,
                    tm = Monadic,
                    ao = AddVal,
                    tf = Arith,
                    d1 = Dest 1 Emulator.Left,
                    d2 = Dest 0 Emulator.Left
                  }
          ),
          ( 2,
            squallAsm $
              Emulator.trace'
                Emulator.Instruction
                  { ea = CodeRelative,
                    er = 2,
                    tm = Dyadic,
                    ao = AddVal,
                    tf = Arith,
                    d1 = Dest 1 Emulator.Left,
                    d2 = Dest 0 Emulator.Left
                  }
          ),
          ( 3,
            squallAsm $
              Emulator.trace'
                Emulator.Instruction
                  { ea = CodeRelative,
                    er = 2,
                    tm = Monadic,
                    ao = Nop,
                    tf = Arith,
                    d1 = Dest 2 Emulator.Left,
                    d2 = Dest 0 Emulator.Left
                  }
          ),
          (4, 1000)
        ]
   in do
        print instrs
        let state =
              ArchState
                { mem = M.fromList $ map (\(a, v) -> (a, (Constant, v))) instrs,
                  pending =
                    [ Token {ctx = 255, stmnt = 0, port = Emulator.Left, val = 42},
                      Token {ctx = 255, stmnt = 0, port = Emulator.Right, val = 58},
                      Token {ctx = 255, stmnt = 1, port = Emulator.Left, val = 28}
                    ]
                }

        print $ clockN 0 state
        print $ clockN 1 state
        print $ clockN 2 state
        print $ clockN 3 state
        print $ clockN 4 state
        print $ clockN 5 state
        print $ clockN 6 state
        print $ clockN 7 state
        print $ clockN 8 state

clockN :: Int -> ArchState -> ArchState
clockN 0 a = a
clockN n a = clockN (n - 1) $ clock squall a

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
      drain          = 35

      -- frame operand slots (er field) for each Dyadic instruction; must be
      -- unique within a frame. Picked clear of the instruction-address range.
      fr_switch  = 200
      fr_final   = 201
      fr_join    = 202
      fr_send_n1 = 203
      fr_send_r1 = 204
      fr_send_n2 = 205
      fr_send_r2 = 206

      -- packed-tag arithmetic constants
      entry_ra_offset = fromIntegral entry_ra * 2 :: AWord -- ENTRY_RA<<1, port L=0
      pow_2_32 = (2 :: AWord) ^ (32 :: Int)
      pow_2_33 = (2 :: AWord) ^ (33 :: Int)
      neg_one  = maxBound :: AWord                          -- 2^64 - 1
      neg_two  = maxBound - 1 :: AWord                      -- 2^64 - 2

      -- helper for Monadic ops where d2 is unused (Nop, Dup-with-single-dest,
      -- Extract). The destination is irrelevant but must encode SOMETHING.
      noDest = Dest 0 Emulator.Left

      mon ao' d1' d2' = Instruction
        { ea = FrameRelative, er = 0
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

      , -- 1  ENTRY_RA: forward RA to final_send.R
        squallAsm $ mon Nop
          (off entry_ra final_send Emulator.Right)
          noDest

      , -- 2  cmp: ConstDyadic Lt N 2 -> switch.R (predicate)
        squallAsm $ cdyadic Lt
          (off cmp switch_addr Emulator.Right)

      , -- 3  cmp_const: 2
        2

      , -- 4  switch: Dyadic Switch. vl=N, vr=(N<2).
        --     True (N<2):  N -> final_send.L (base case: fib(0)=0, fib(1)=1)
        --     False:       N -> rec_entry.L (recursive case)
        squallAsm $ dyadic fr_switch Switch Nop
          (off switch_addr final_send Emulator.Left)
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
          { ea = FrameRelative, er = 0
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
        --     Computes tag1_high = (2*self_ctx) << 32, i.e. the high bits
        --     of pack(2*self_ctx, _, _). 2*self_ctx is the new ctx for child 1.
        squallAsm $ cdyadic MulVal
          (off mul_2_33 tag1_dup1 Emulator.Left)

      , -- 17 mul_2_33_const: 2^33
        pow_2_33

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

      , -- 20 add_2_32: tag1_high + 2^32 = tag2_high (since new_ctx2 = new_ctx1+1
        --     and shifting that left 32 adds 2^32 to the shifted form). -> tag2_dup
        squallAsm $ cdyadic AddVal
          (off add_2_32 tag2_dup Emulator.Left)

      , -- 21 add_2_32_const: 2^32
        pow_2_32

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
          { ea = FrameRelative, er = 0
          , tm = Monadic, ao = Nop, tf = Extract
          , d1 = off mint_ret1 send_ra1 Emulator.Right
          , d2 = off mint_ret1 ret_join Emulator.Left
          }

      , -- 28 mint_ret2: same but encodes (self_ctx, ret_join, R) so child 2's
        --     result lands on the OTHER port of ret_join.
        squallAsm $ Instruction
          { ea = FrameRelative, er = 0
          , tm = Monadic, ao = Nop, tf = Extract
          , d1 = off mint_ret2 send_ra2 Emulator.Right
          , d2 = off mint_ret2 ret_join Emulator.Right
          }

      , -- 29 send_n1: Dyadic Send. L=tag1_for_N, R=N-1. Sends N-1 to child 1's
        --     ENTRY_N. Ack drains.
        squallAsm $ dyadic fr_send_n1 Send Nop
          (off send_n1 drain Emulator.Left)
          noDest

      , -- 30 send_ra1: L=tag1_for_RA, R=ret-tag-1. Sends RA to child 1's ENTRY_RA.
        squallAsm $ dyadic fr_send_r1 Send Nop
          (off send_ra1 drain Emulator.Left)
          noDest

      , -- 31 send_n2: child 2 N-arg
        squallAsm $ dyadic fr_send_n2 Send Nop
          (off send_n2 drain Emulator.Left)
          noDest

      , -- 32 send_ra2: child 2 RA
        squallAsm $ dyadic fr_send_r2 Send Nop
          (off send_ra2 drain Emulator.Left)
          noDest

      , -- 33 ret_join: Dyadic AddVal. L=ret1 (from child 1's Send), R=ret2.
        --     -> final_send.L (the recursive case's result)
        squallAsm $ dyadic fr_join Arith AddVal
          (off ret_join final_send Emulator.Left)
          noDest

      , -- 34 final_send: Dyadic Send. L=result (base or recursive), R=RA.
        --     Sends result at RA tag. This is the single spec-compliant
        --     return-address handler that both code paths route through.
        squallAsm $ dyadic fr_final Send Nop
          (off final_send drain Emulator.Left)
          noDest

      , -- 35 drain: Send-ack sink. Self-loop but never produces useful tokens
        --     (the drain itself is Monadic and just consumes acks).
        squallAsm $ mon Nop noDest noDest
      ]