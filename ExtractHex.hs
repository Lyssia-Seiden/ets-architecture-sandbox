import Data.Bits (Bits (shiftL), (.&.), (.|.))
import Data.IntMap qualified as M
import Data.Maybe (fromMaybe)
import Emulator
import Numeric (showHex)
import Programs
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [programName, nStr, outputDir] -> do
      let n = read nStr :: Int
      let maxCycles = 100000
      let resultAddr = 10000
      let programs =
            [ ("fib", initialFibState n),
              ("sum", initialSumState n)
            ]
      let initState = fromMaybe (error $ programName ++ " is not a valid program!") $ lookup programName programs
      let finalState = runUntilQuiescent maxCycles initState
      let (_, resultVal) = M.findWithDefault (Empty, 0) resultAddr (mem finalState)

      createDirectoryIfMissing True outputDir
      let writeTo name content = do
            h <- openFile (outputDir ++ "/" ++ name) WriteMode
            hPutStr h content
            hClose h

      writeTo "memory.hex" (initMemory $ mem initState)
      writeTo "presence_bits.hex" (initPB $ mem initState)
      writeTo "tokens.hex" (initTokens $ pending initState)
      writeTo "params.txt" $
        unlines
          [ "num_tokens = " ++ show (length $ pending initState),
            "result_addr = " ++ show resultAddr,
            "expected_result = " ++ showHex resultVal "",
            "max_cycles = " ++ show maxCycles
          ]

      putStrLn $ "Exported " ++ programName ++ "(" ++ show n ++ ") to " ++ outputDir
      putStrLn $ "Expected result: " ++ showHex resultVal ""
    _ -> do
      hPutStrLn stderr "Usage: ExtractHex <fib|sum> <N> <output_dir>"

data Mems = Mems {memory :: String, presenceBits :: String, tokens :: String} deriving (Show)

initMemory :: Emulator.Memory -> String
initMemory = M.foldlWithKey (\l k (_, v) -> l ++ ('@' : showHex k (' ' : showHex v "\n"))) ""

initPB :: Emulator.Memory -> String
initPB = M.foldlWithKey (\l k (pb, _) -> l ++ ('@' : showHex k (' ' : showHex (squallAsmPB pb) "\n"))) ""

initTokens :: [Emulator.Token] -> String
initTokens =
  foldl
    ( \s token ->
        let v = fromIntegral (val token) :: Integer
            p = if port token == Emulator.Left then 0 else 1 :: Integer
            st = fromIntegral (stmnt token) .&. (2 ^ 32 - 1) :: Integer
            cx = fromIntegral (ctx token) .&. (2 ^ 32 - 1) :: Integer
            packed = (cx `shiftL` 97) .|. (st `shiftL` 65) .|. (p `shiftL` 64) .|. v
         in s ++ showHex packed "\n"
    )
    ""
