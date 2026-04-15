import Assembler
import Data.IntMap qualified as M
import Data.Map.Strict qualified as Map
import Emulator
import Programs

main :: IO ()
main = do
  -- Heap-indexed ctx allocation; depth limit ~29 (32-bit ctx field).
  -- Test fib(0..16); higher values are correct but slow (naive O(fib(n)) recursion).
  let fibExpected = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 377 + 610]
  mapM_ (\(n, e) -> do
    let r = fibTest n
    putStrLn $ "fib " ++ show n ++ " = " ++ show r
              ++ if r == e then "" else "  *** EXPECTED " ++ show e
    ) (zip [0..] fibExpected)

  -- Test sum(0..20); sum(N) = N*(N+1)/2. Linear recursion, depth=N.
  let sumExpected = [n * (n + 1) `div` 2 | n <- [0..20]]
  mapM_ (\(n, e) -> do
    let r = sumTest n
    putStrLn $ "sum " ++ show n ++ " = " ++ show r
              ++ if r == fromIntegral e then "" else "  *** EXPECTED " ++ show e
    ) (zip [0..] sumExpected)

sumTest :: Int -> AWord
sumTest n =
  let final = Programs.runUntilQuiescent 1000000 (Programs.initialSumState n)
   in case mem final M.!? 10000 of
        Just (_, v) -> v
        Nothing     -> error "sumTest: result slot never written"

fibTest :: Int -> AWord
fibTest n =
  let final = Programs.runUntilQuiescent 1000000 (Programs.initialFibState n)
   in case mem final M.!? 10000 of
        Just (_, v) -> v
        Nothing     -> error "fibTest: result slot never written"
