import Control.Monad (when)
import Data.Bifunctor qualified
import Data.Bits (Bits (shiftL), (.&.), (.|.))
import Data.IntMap qualified as M
import Data.List (elemIndex, findIndex)
import Data.Maybe (fromMaybe)
import Emulator
import Numeric (showHex)
import Programs
import System.Directory (createDirectory, createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitSuccess, exitWith)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let optParams = [("--num_tokens", show 2), ("--result_addr", show 10000), ("--max_cycles", show 100000)]
  let reqParams = ["--expected_result", "--output_dir"]
  when ("--help" `elem` args) $ do
    print $ "Optional parameters:\n" ++ foldl (\l s -> l ++ s ++ "\n") "" (map (\(s, _) -> "\t" ++ s ++ "\n") optParams)
    exitSuccess
  -- - params.txt: num_tokens=2, result_addr=10000, expected_result=<hex>, max_cycles=100000
  let optParamPairs =
        map
          (\(name, d) -> (name, maybe d ((args !!) . (+ 1)) (elemIndex name args)))
          optParams
  let reqParamPairs =
        map
          ( \name ->
              ( name,
                maybe
                  (error $ "Must supply a value for " ++ name)
                  ((args !!) . (+ 1))
                  (elemIndex name args)
              )
          )
          reqParams
  let params = optParamPairs ++ reqParamPairs :: [(String, String)]

  let programName = head args
  let programInput = read $ args !! 1
  let programs = [("fib", Programs.initialFibState programInput), ("sum", Programs.initialSumState programInput)]
  let program = fromMaybe (error $ programName ++ " is not a valid program!") $ lookup programName programs
  let mems = asToMems program
  -- unused for now
  let expected = runUntilQuiescent (read $ fromMaybe (error "Max cycles not specified") $ lookup "--max_cycles" params) program
  -- TODO the return address is not used by the program?

  createDirectoryIfMissing True $ fromMaybe (error "No output dir") (lookup "--output_dir" params)

  let getHandle name =
        openFile
          (fromMaybe (error "No output dir") (lookup "--output_dir" params) ++ '/' : name)
          WriteMode

  paramsFile <- getHandle "params.txt"
  let stripDashes ('-' : '-' : s) = s
  let stripDashes s = s

  hPutStr paramsFile $
    foldl
      (\s (name, val) -> s ++ name ++ " = " ++ show val ++ "\n")
      ""
      (map (Data.Bifunctor.first stripDashes) params)

  memFile <- getHandle "memory.hex"
  hPutStr memFile $ memory mems

  pbFile <- getHandle "presence_bits.hex"
  hPutStr pbFile $ presenceBits mems

  tokenFile <- getHandle "tokens.hex"
  hPutStr tokenFile $ tokens mems

  hClose paramsFile
  hClose memFile
  hClose pbFile
  hClose tokenFile

  return ()

data Mems = Mems {memory :: String, presenceBits :: String, tokens :: String} deriving (Show)

asToMems :: ArchState -> Mems
asToMems as = Mems (initMemory $ mem as) (initPB $ mem as) (initTokens $ pending as)

initMemory :: Emulator.Memory -> String
initMemory = M.foldlWithKey (\l k (_, v) -> l ++ ('@' : showHex k (' ' : showHex v "\n"))) ""

initPB :: Emulator.Memory -> String
initPB = M.foldlWithKey (\l k (pb, _) -> l ++ ('@' : showHex k (' ' : showHex (squallAsmPB pb) "\n"))) ""

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
