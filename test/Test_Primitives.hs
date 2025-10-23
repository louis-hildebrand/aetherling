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

test_lut_gen :: IO ()
test_lut_gen = do
  let count = Counter_tN 4 0 3 UInt8T 0 (Index 1)
  let lut = fmap (\x -> Int32V (5 * x)) [0..15]
  let map = Map_tN 4 0 (Lut_GenN lut Int32T (InputN UInt8T "I" (Index 2)) (Index 3)) count (Index 4)
  -- The Chisel backend apparently doesn't support Lut_GenN.
  --
  -- Aetherling> test (suite: Aetherling-Tests, args: --num-threads 1)
  --
  -- Progress 1/2: AetherlingAetherling-Tests: don't support yet: Lut_GenN {lookup_table = [Int32V 0,Int32V 5,Int32V 10,Int32V 15,Int32V 20,Int32V 25,Int32V 30,Int32V 35,Int32V 40,Int32V 45,Int32V 50,Int32V 55,Int32V 60,Int32V 65,Int32V 70,Int32V 75], lookup_types = Int32T, seq_in = InputN {t = UInt8T, input_name = "I", index = Index 2}, index = Index 3}
  emit_text map (wrap_single_t 1) "lut"

test_tuple_values :: IO ()
test_tuple_values = do
  let v = ATupleV (ATupleV (BitV False) (BitV True))
                  (STupleV [UInt8V 42, UInt8V 26])
  let e = Const_GenN v (ATupleT (ATupleT BitT BitT) (STupleT 2 UInt8T)) 0 (Index 1)
  emit e "tuple_values"

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

test_unpartition_t :: IO ()
test_unpartition_t = do
  let no = 3
  let io = 0
  let ni = 1
  let ii = 1
  let count = Counter_tnN [no, ni] [io, ii] 1 UInt16T 0 (Index 1)
  let joined = Unpartition_t_ttN no ni io ii UInt16T count (Index 2)
  emit joined "unpartition_t"

test_remove_1_t :: IO ()
test_remove_1_t = do
  let int = Int8T
  let sv = TSeqV { vals = [TSeqV { vals = [Int8V (-42), Int8V 0, Int8V 9],
                                   i_v = 0 }],
                   i_v = 0 }
  let s = Const_GenN sv (TSeqT 1 0 (TSeqT 3 0 int)) 0 (Index 1)
  let f = AbsN int (InputN int "I" $ Index 2) (Index 3)
  let map_f = Map_tN 3 0 f (InputN (TSeqT 3 0 int) "I" (Index 4)) (Index 5)
  let removed = Remove_1_0_tN map_f s (Index 6)
  emit removed "remove_1_t"
