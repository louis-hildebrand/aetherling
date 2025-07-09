import Test.Tasty
import Test.Tasty.HUnit
import Test_Seq_Simulator
import Test_Seq_Embedding
import Test_ST_Embedding
import Test_Slowdown
import Test_Apps
import Test_Apps_Real_Math
import Test_Big
import Test_Big16
import Test_Big32
import Test_Spatial
import Test_Big_Real_Math
import Test_Big_Real_Math16
import Test_Big_Real_Math32
import Test_Demosaic
import Test_Camera
import Test_More_Simple_Benchmarks
import Aetherling.Languages.Space_Time.Deep.Expr_Type_Conversions
import Aetherling.Languages.Space_Time.Deep.Types
import Aetherling.Rewrites.Sequence_To_Partially_Parallel_Space_Time.Rewrite_All_Types
import Aetherling.Interpretations.Backend_Execute.Test_Helpers
import Aetherling.Interpretations.Space_Time_Printer
import Aetherling.Interpretations.Backend_Execute.Value_To_String
import Data.List

main :: IO ()
main = do
  print_st_text
  print_verilog

print_st_text :: IO ()
print_st_text = do
  single_reduce_sum_st_prints
  single_map_200_st_prints
  -- conv_2d_st_prints
  -- conv_2d_b2b_print_st
  -- sharpen_print_st
  -- camera_st_prints
  return ()

print_verilog :: IO ()
print_verilog = do
  single_reduce_sum_chisel_prints
  single_map_200_chisel_prints
  return ()
