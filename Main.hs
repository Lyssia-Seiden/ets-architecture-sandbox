{-# LANGUAGE NumericUnderscores #-}
import Data.IntMap qualified as M
import Emulator

main = do
  let instrs = [(0, 00000000000000000010_0_00000000000000000000_0_0000000000_00_00_00000_00)] :: [(Int, Emulator.AWord)]
  let state =
        ArchState
          { mem = M.insert 0 (Present, 0) M.empty,
            pending =
              [ Token {ctx = 1, stmnt = 0, port = Emulator.Left, val = 42},
                Token {ctx = 1, stmnt = 0, port = Emulator.Right, val = 58}
              ]
          }

  print state
  print $ clock squall state
