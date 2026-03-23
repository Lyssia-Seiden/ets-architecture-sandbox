import Data.IntMap qualified as M
import Emulator

main = do
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
