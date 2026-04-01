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

clockN :: Int -> ArchState -> ArchState
clockN 0 a = a
clockN n a = clockN (n - 1) $ clock squall a