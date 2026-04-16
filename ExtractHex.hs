import Data.Bits (Bits (shiftL), (.&.), (.|.))
import Data.IntMap qualified as M
import Emulator
import Numeric (showHex)

data Mems = Mems {memory :: String, presence_bits :: String, tokens :: String}

asToMems :: ArchState -> Mems
asToMems as = Mems (initMemory $ mem as) (initPB $ mem as) (initTokens $ pending as)

initMemory :: Emulator.Memory -> String
initMemory = M.foldlWithKey (\l k (_, v) -> l ++ ('@' : showHex k (showHex v "\n"))) ""

initPB :: Emulator.Memory -> String
initPB = M.foldlWithKey (\l k (pb, _) -> l ++ ('@' : showHex k (showHex (squallAsmPB pb) "\n"))) ""

initTokens :: [Emulator.Token] -> String
initTokens =
  foldl
    ( \s token ->
        s
          ++ show
            ( val token
                .|. if port token == Emulator.Left
                  then 0
                  else
                    1 `shiftL` 64
                      .|. ((fromIntegral (stmnt token) `shiftL` 65) .&. (2 ^ 31 - 1))
                      .|. ((fromIntegral (ctx token) `shiftL` 97) .&. (2 ^ 32 - 1))
            )
    )
    []
