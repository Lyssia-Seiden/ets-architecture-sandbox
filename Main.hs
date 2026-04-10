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

squallFactorial :: [AWord]
-- token N on instruction 0
-- if N in {0, 1}, send 0 to res
-- else:
-- construct ctx1 = (2 * ctx) -- this will be several instructions
-- construct ctx2 = (2 * ctx + 5) -- this will be several instructions
-- send the add addr and (N - 1) to the starting instruction, with ctx1
-- send the add addr and (N - 2) to the starting instruction, with ctx2
-- add
-- send res to ra
squallFactorial =
  [ squallAsm $
      Emulator.Instruction
        { ea = FrameRelative,
          er = 0,
          tm = Monadic,
          ao = Dup,
          tf = Arith,
          d1 = Dest 1 Emulator.Left,
          d2 = Dest 6 Emulator.Left
        },
    squallAsm $
      Emulator.Instruction
        { ea = FrameRelative,
          er = 0,
          tm = Monadic,
          ao = Dup,
          tf = Arith,
          d1 = Dest 1 Emulator.Left,
          d2 = Dest 8 Emulator.Left
        },
    -- N < 2
    squallAsm $
      Emulator.Instruction
        { ea = CodeRelative,
          er = 1,
          tm = ConstDyadic,
          ao = Lt,
          tf = Arith,
          d1 = Dest 2 Emulator.Left,
          d2 = Dest 0 Emulator.Left
        },
    2,
    -- duplicate (N < 2)
    squallAsm $
      Emulator.Instruction
        { ea = FrameRelative,
          er = 0,
          tm = Monadic,
          ao = Dup,
          tf = Arith,
          d1 = Dest 1 Emulator.Left,
          d2 = Dest 1 Emulator.Right
        },
    -- if (N < 2) issue d1, else d2
    squallAsm $
      Emulator.Instruction
        { ea = FrameRelative,
          er = 0,
          tm = Dyadic,
          ao = Nop,
          tf = Switch,
          d1 = Dest 20 Emulator.Left, -- return 1
          d2 = Dest 1 Emulator.Right -- recurse
        },
    -- dup N for recursion
    squallAsm $
      Emulator.Instruction
        { ea = FrameRelative,
          er = 0,
          tm = Dyadic,
          ao = Dup,
          tf = Arith,
          d1 = Dest 1 Emulator.Left, -- recurse N - 1
          d2 = Dest 4 Emulator.Right -- recurse N - 2
        },
    -- decr 1
    squallAsm $
      Emulator.Instruction
        { ea = CodeRelative,
          er = 1,
          tm = ConstDyadic,
          ao = AddVal,
          tf = Arith,
          d1 = Dest 2 Emulator.Left,
          d2 = Dest 0 Emulator.Left
        },
    fromIntegral $ 2 ^ 64 - 1, -- -1
    -- calc new ctx
    squallAsm $
      Emulator.Instruction
        { ea = CodeRelative,
          er = 0,
          tm = Monadic,
          ao = Dup,
          tf = Arith,
          d1 = Dest 1 Emulator.Left,
          d2 = Dest 0 Emulator.Left
        },
    squallAsm $
      Emulator.Instruction
        { ea = CodeRelative,
          er = 1,
          tm = ConstDyadic,
          ao = MulVal, -- TODO use bitshift?
          tf = Arith,
          d1 = Dest 2 Emulator.Left,
          d2 = Dest 0 Emulator.Left
        },
    2
  ]