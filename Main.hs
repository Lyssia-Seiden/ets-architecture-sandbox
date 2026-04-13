import Assembler
import Data.IntMap qualified as M
import Data.Map.Strict qualified as Map
import Emulator

main :: IO ()
main = do
  -- Heap-indexed ctx allocation; depth limit ~29 (32-bit ctx field).
  -- Test fib(0..16); higher values are correct but slow (naive O(fib(n)) recursion).
  let expected = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 377 + 610]
  mapM_ (\(n, e) -> do
    let r = fibTest n
    putStrLn $ "fib " ++ show n ++ " = " ++ show r
              ++ if r == e then "" else "  *** EXPECTED " ++ show e
    ) (zip [0..] expected)

-- Run the machine until pending is empty or the cycle cap is hit.
runUntilQuiescent :: Int -> ArchState -> ArchState
runUntilQuiescent 0 s = s
runUntilQuiescent n s
  | null (pending s) = s
  | otherwise        = runUntilQuiescent (n - 1) (clock squall s)

(fibSlots, fibCode) = assemble squallFibNodes

initialFibState :: Int -> ArchState
initialFibState n =
  let prog       = zip [0..] fibCode
      raCtx      = 10000
      raStmnt    = length fibCode + 10  -- outside program range
      raTag      = squallPackTag (fromIntegral raCtx) (fromIntegral raStmnt) Emulator.Left
      initialCtx = 8   -- heap index 1 × spacing 8
      memInit    = M.fromList [(a, (Constant, v)) | (a, v) <- prog]
      slot name  = fibSlots Map.! name
   in ArchState
        { mem = memInit
        , pending =
            [ Token { ctx = initialCtx, stmnt = slot "entry_n",  port = Emulator.Left, val = fromIntegral n }
            , Token { ctx = initialCtx, stmnt = slot "entry_ra", port = Emulator.Left, val = raTag }
            ]
        }

fibTest :: Int -> AWord
fibTest n =
  let final = runUntilQuiescent 1000000 (initialFibState n)
   in case mem final M.!? 10000 of
        Just (_, v) -> v
        Nothing     -> error "fibTest: result slot never written"

-- Naive Fibonacci: fib(N) = if N<2 then N else fib(N-1) + fib(N-2)
--
-- Calling convention (2 input tokens per invocation):
--   N  arrives at "entry_n"  (port L)
--   RA arrives at "entry_ra" (port L) -- packed return-address tag
-- Returns by Send-ing one token at the RA tag carrying fib(N).
squallFibNodes :: [Node]
squallFibNodes =
  let neg_one  = maxBound :: AWord
      neg_two  = maxBound - 1 :: AWord
      -- Heap-indexed: child1 = 2*self_ctx, child2 = 2*self_ctx + 8.
      -- Spacing=8 >= frame footprint (7). Max depth ~29.
      sib_mul  = (2 :: AWord) ^ (33 :: Int)
      sib_add  = (2 :: AWord) ^ (35 :: Int)
      -- entry_ra is at slot 1 (second node, 1-slot Monadic).
      -- Tag offset = slot*2 + port_bit = 1*2+0 = 2.
      ra_off   = 2 :: AWord
      r = Ref
      l = Emulator.Left
      ri = Emulator.Right
  in
  [ Node "entry_n"      $ Mono Nop    [r "cmp" l, r "switch" l]
  , Node "entry_ra"     $ Mono Nop    [r "final_send" l]
  , Node "cmp"          $ CDyadic Lt 2 [r "switch" ri]
  , Node "switch"       $ Sw [r "final_send" ri] [r "rec_entry" l]
  , Node "rec_entry"    $ Mono Nop    [r "decr1" l, r "decr2" l, r "ctx_extract" l,
                                        r "mint_ret1" l, r "mint_ret2" l]
  , Node "decr1"        $ CDyadic AddVal neg_one  [r "send_n1" ri]
  , Node "decr2"        $ CDyadic AddVal neg_two  [r "send_n2" ri]
  , Node "ctx_extract"  $ Ext [r "ctx_shr" l] (r "ctx_shr" l)
  , Node "ctx_shr"      $ CDyadic Shr 32          [r "ctx_mul" l]
  , Node "ctx_mul"      $ CDyadic MulVal sib_mul  [r "tag2_add" l, r "send_n1" l, r "ra1_add" l]
  , Node "tag2_add"     $ CDyadic AddVal sib_add  [r "send_n2" l, r "ra2_add" l]
  , Node "ra1_add"      $ CDyadic AddVal ra_off   [r "send_ra1" l]
  , Node "ra2_add"      $ CDyadic AddVal ra_off   [r "send_ra2" l]
  , Node "mint_ret1"    $ Ext [r "send_ra1" ri] (r "ret_join" l)
  , Node "mint_ret2"    $ Ext [r "send_ra2" ri] (r "ret_join" ri)
  , Node "send_n1"      Snd
  , Node "send_ra1"     Snd
  , Node "send_n2"      Snd
  , Node "send_ra2"     Snd
  , Node "ret_join"     $ DyadicA AddVal [r "final_send" ri]
  , Node "final_send"   Snd
  ]
