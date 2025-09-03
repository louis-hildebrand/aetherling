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

-- TODO: Remove_1_sN
-- TODO: Shift_tnN
-- TODO: Partition_s_ssN
-- TODO: Partition_t_ttN
-- TODO: Unpartition_s_ssN
-- TODO: Unpartition_t_ttN
-- TODO: SerializeN
