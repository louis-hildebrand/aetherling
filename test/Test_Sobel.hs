module Test_Sobel where
import Aetherling.Interpretations.Backend_Execute.Compile
import Aetherling.Languages.Isomorphisms
import Aetherling.Languages.Sequence.Shallow.Expr
import Aetherling.Languages.Sequence.Shallow.Types
import Aetherling.Rewrites.Sequence_Shallow_To_Deep
import Data.Proxy
import Data.Ratio
import Data.Int

x_kernel :: [[Int32]]
x_kernel = [[-1, 0, 1],
            [-2, 0, 2],
            [-1, 0, 1]]
y_kernel :: [[Int32]]
y_kernel = [[-1, -2, -1],
            [ 0,  0,  0],
            [ 1,  2,  1]]

stencil_3_1dC_nested in_seq = do
  let shifted_once = shiftC (Proxy @1) in_seq
  let shifted_twice = shiftC (Proxy @1) shifted_once
  let window_tuple = map2C seq_tuple_appendC
                     (map2C seq_tupleC shifted_twice shifted_once)
                     in_seq
  let partitioned_tuple = partitionC Proxy (Proxy @1) window_tuple
  mapC seq_tuple_to_seqC partitioned_tuple

stencil_3x3_2dC_test in_col in_img = do
  let first_row = in_img
  let second_row = shiftC in_col in_img
  let third_row = shiftC in_col second_row
  let first_row_shifted = stencil_3_1dC_nested first_row
  let second_row_shifted = stencil_3_1dC_nested second_row
  let third_row_shifted = stencil_3_1dC_nested third_row
  let tuple = map2C seq_tupleC third_row_shifted second_row_shifted
  let triple = map2C seq_tuple_appendC tuple first_row_shifted
  let partitioned_triple = partitionC Proxy (Proxy @1) triple
  mapC seq_tuple_to_seqC partitioned_triple

tuple_2d_mul_shallow_no_input kernel in_seq = do
  let kernel_list = list_to_seq (Proxy @3) $
                    fmap (list_to_seq (Proxy @3)) $
                    fmap (fmap Atom_Int32) kernel
  let kernel = const_genC kernel_list in_seq
  let kernel_and_values = map2C (map2C atom_tupleC) in_seq kernel
  let mul_result = mapC (mapC mulC) kernel_and_values
  let sum = reduceC'' (mapC addC) $ mapC (reduceC addC) mul_result
  sum

my_sqrt in_seq = do
  let init n = let lo = const_genC (Atom_Int32 0) n in
               let one32 = const_genC (Atom_Int32 1) n in
               let one8 = const_genC (Atom_UInt8 1) n in
               let half = lsrC $ atom_tupleC n one8 in
               let hi = addC $ atom_tupleC half one32 in
               atom_tupleC n (atom_tupleC lo hi)
  let step x = let n = fstC x in
               let lo = fstC $ sndC x in
               let hi = sndC $ sndC x in
               let lo_plus_hi = addC $ atom_tupleC lo hi in
               let one8 = const_genC (Atom_UInt8 1) n in
               let one32 = const_genC (Atom_Int32 1) n in
               let mid0 = lsrC $ atom_tupleC lo_plus_hi one8 in
               let mid0_plus_one = addC $ atom_tupleC mid0 one32 in
               let mid1 = ifC $ atom_tupleC (eqC $ atom_tupleC mid0 lo) $ atom_tupleC mid0_plus_one mid0 in
               let mid_sq = mulC $ atom_tupleC mid1 mid1 in
               let c = ltC $ atom_tupleC n mid_sq in
               let out_then = atom_tupleC lo (subC $ atom_tupleC mid1 one32) in
               let out_else = atom_tupleC mid1 hi in
               let new_lo_hi = ifC $ atom_tupleC c $ atom_tupleC out_then out_else in
               atom_tupleC n new_lo_hi
  let n_lo_hi_0 = mapC init in_seq
  let n_lo_hi_32 = foldr (\_ -> mapC step) n_lo_hi_0 [1..32]
  mapC (\x -> fstC $ sndC x) n_lo_hi_32

sobel in_col in_seq = do
  let x_stencil = stencil_3x3_2dC_test in_col in_seq
  let y_stencil = stencil_3x3_2dC_test in_col in_seq
  let x_conv = unpartitionC $ unpartitionC $
        mapC (tuple_2d_mul_shallow_no_input x_kernel) x_stencil
  let y_conv = unpartitionC $ unpartitionC $
        mapC (tuple_2d_mul_shallow_no_input y_kernel) y_stencil
  let x_conv_squared = mapC (\x -> mulC $ atom_tupleC x x) x_conv
  let y_conv_squared = mapC (\y -> mulC $ atom_tupleC y y) y_conv
  let norm_squared = map2C (\x -> \y -> addC $ atom_tupleC x y) x_conv_squared y_conv_squared
  let norm = my_sqrt norm_squared
  norm

sobel_bench = sobel (Proxy @1920) $ com_input_seq "I" (Proxy :: Proxy (Seq 2073600 Atom_Int32))

sobel_throughputs = [1%3, 1, 2, 4, 8, 16]

sobel_st_prints = sequence $
  fmap (\s -> compile_to_file
              sobel_bench (wrap_single_t s)
              text_backend "bigsobel")
  sobel_throughputs

sobel_chisel_prints = sequence $
  fmap (\s -> compile_to_file
              sobel_bench (wrap_single_t s)
              Chisel "bigsobel")
  sobel_throughputs
