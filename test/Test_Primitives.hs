module Test_Primitives where

import Aetherling.Languages.Space_Time.Deep.Expr
import Aetherling.Languages.Space_Time.Deep.Types
import Aetherling.Interpretations.Backend_Execute.Compile
import Aetherling.Monad_Helpers
import Control.Monad.Except

emit_text :: Expr -> Throughput_Target -> String -> IO ()
emit_text e s_target output_name_template = do
  result <- runExceptT $ compile_to_text [e] s_target output_name_template
  case result of
    Left x -> error $ "Compiler Error: " ++ show x
    Right x -> return ()

emit_chisel :: Expr -> Throughput_Target -> String -> IO ()
emit_chisel e s_target output_name_template = do
  result <- runExceptT $ compile_to_chisel [e] s_target output_name_template
  case result of
    Left x -> error $ "Compiler Error: " ++ show x
    Right x -> return ()

emit :: Expr -> String -> IO ()
emit e name = do
  emit_text e (wrap_single_t 1) name
  emit_chisel e (wrap_single_t 1) name

test_counter_ts :: IO ()
test_counter_ts = do
  let no = 10
  let io = 0
  let ni = 7
  let delta = 3
  let count = Counter_tsN no io ni delta UInt8T 0 (Index 1)
  emit count "count_ts"

test_counter_tn :: IO ()
test_counter_tn = do
  let ns = [6, 1]
  let is = [0, 3]
  let delta = 2
  let count = Counter_tnN ns is delta UInt8T 0 (Index 1)
  emit count "count_tn"

-- The Chisel backend apparently doesn't support Shift_ttN.
-- (I guess Shift_tnN is more general, so you might as well always use that one.)
--
-- Aetherling> test (suite: Aetherling-Tests, args: --num-threads 1)
--
-- Aetherling-Tests: don't support yet: Shift_ttN {no = 6, ni = 2, io = 0, ii = 1, shift_amount = 1, elem_t = UInt8T, seq_in = Counter_tnN {ns = [6,2], is = [0,1], incr_amount = 3, int_type = UInt8T, delay = 0, index = Index 1}, index = Index 2}
-- CallStack (from HasCallStack):
--   error, called at src/Core/Aetherling/Interpretations/Backend_Execute/Chisel/Expr_To_String.hs:749:37 in Aetherling-0.1.0.0-Hwg5g5UjHrjBgqUpk6qqqi:Aetherling.Interpretations.Backend_Execute.Chisel.Expr_To_String
test_shift_tt :: IO ()
test_shift_tt = do
  return ()
  -- let no = 6
  -- let ni = 2
  -- let io = 0
  -- let ii = 1
  -- let delta = 3
  -- let typ = UInt8T
  -- let count = Counter_tnN [no, ni] [io, ii] delta typ 0 (Index 1)
  -- let shift_amount = 1
  -- let shifted = Shift_ttN no ni io ii shift_amount typ count (Index 2)
  -- emit shifted "shift_tt"

test_shift_tn :: IO ()
test_shift_tn = do
  let ns = [6, 1]
  let is = [0, 2]
  let delta = 3
  let typ = UInt8T
  let count = Counter_tnN ns is delta typ 0 (Index 1)
  let shift_amount = 1
  let shifted = Shift_tnN (head ns) (tail ns) (head is) (tail is) shift_amount typ count (Index 2)
  emit shifted "shift_tn"

test_up_1d_s :: IO ()
test_up_1d_s = do
  let v = Const_GenN (SSeqV [Int16V (-42)]) (SSeqT 1 Int16T) 0 (Index 1)
  let up = Up_1d_sN 8 Int16T v (Index 2)
  emit up "up_1d_s"

test_up_1d_t :: IO ()
test_up_1d_t = do
  let s = Const_GenN (TSeqV {vals = [Int16V (42)], i_v = 7}) (TSeqT 1 7 Int16T) 0 (Index 1)
  let up = Up_1d_tN 4 4 Int16T s (Index 2)
  emit up "up_1d_t"

test_down_1d_s :: IO ()
test_down_1d_s = do
  let n = 8
  let idx = 2
  let count = Counter_sN n 3 UInt8T 0 (Index 1)
  let down = Down_1d_sN n idx UInt8T count (Index 2)
  emit down "down_1d_s"

test_down_1d_t :: IO ()
test_down_1d_t = do
  let n = 8
  let i = 0
  let idx = 2
  let count = Counter_tN n i 3 UInt8T 0 (Index 1)
  let down = Down_1d_tN n i idx UInt8T count (Index 2)
  emit down "down_1d_t"
