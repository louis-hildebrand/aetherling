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
import Test_Camera_Small
import Test_More_Simple_Benchmarks
import Test_Primitives
import Test_Sobel
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
  -- dump_camera_outputs
  -- dump_small_camera_outputs
  -- test_primitives
  -- small_mvm_st_prints

print_st_text :: IO ()
print_st_text = do
  big_jacobi_st_prints
  big_mmm_st_prints
  small_mmm_st_prints
  big_mvm_st_prints
  small_mvm_st_prints
  sobel_st_prints
  sqrt_st_prints
  small_camera_st_prints
  big_camera_st_prints
  big_sharpen_st_prints
  big_convb2b_st_prints
  big_conv2d_st_prints
  small_sharpen_st_prints
  big_conv_2d_st_prints
  conv1d_st_prints
  dot_prod_st_prints
  single_reduce_sum_st_prints
  single_map_200_st_prints
  return ()

print_verilog :: IO ()
print_verilog = do
  big_jacobi_chisel_prints
  big_mmm_chisel_prints
  small_mmm_chisel_prints
  big_mvm_chisel_prints
  small_mvm_chisel_prints
  sobel_chisel_prints
  sqrt_chisel_prints
  small_camera_chisel_prints
  big_camera_chisel_prints
  big_sharpen_chisel_prints
  big_convb2b_chisel_prints
  big_conv2d_chisel_prints
  small_sharpen_chisel_prints
  small_conv_2d_b2b_chisel_prints
  small_conv_2d_chisel_prints
  conv1d_chisel_prints
  dot_prod_chisel_prints
  single_reduce_sum_chisel_prints
  single_map_200_chisel_prints
  return ()

test_primitives :: IO ()
test_primitives = do
  test_lut_gen
  test_tuple_values
  test_counter_ts
  test_counter_tn
  test_shift_tt
  test_shift_tn
  test_up_1d_s
  test_up_1d_t
  test_down_1d_s
  test_down_1d_t
  test_unpartition_t
  test_remove_1_t
