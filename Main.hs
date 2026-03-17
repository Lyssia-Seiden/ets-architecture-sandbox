import Emulator
import Data.IntMap qualified as M

main = do
    let state = ArchState { mem = M.empty, pending = [Token 42 67 1337] }

    print state
    print $ clock state
