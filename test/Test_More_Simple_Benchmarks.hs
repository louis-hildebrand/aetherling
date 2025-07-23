module Test_More_Simple_Benchmarks where
import Aetherling.Interpretations.Backend_Execute.Compile
import Aetherling.Languages.Isomorphisms
import Aetherling.Languages.Sequence.Shallow.Expr
import Aetherling.Languages.Sequence.Shallow.Types
import Aetherling.Rewrites.Sequence_Shallow_To_Deep
import Data.Proxy
import Data.Ratio

-- sum of the elements in a Seq
--------------------------------------------------------------------------------

single_reduce_sum =
  reduceC' (Proxy @840) addC $
  com_input_seq "I" (Proxy :: Proxy (Seq 840 Atom_UInt32))

single_reduce_throughputs = map (\t -> t % 840) [1, 2, 3, 4, 5, 6, 7, 8]

single_reduce_sum_st_prints = sequence $
  fmap (\s -> compile_to_file
              single_reduce_sum (wrap_single_t s)
              text_backend "sum")
  single_reduce_throughputs

single_reduce_sum_chisel_prints = sequence $
  fmap (\s -> compile_to_file
              single_reduce_sum (wrap_single_t s)
              Chisel "sum")
  single_reduce_throughputs

-- dot product
--------------------------------------------------------------------------------

curried_mul x y = do
  let tupled = atom_tupleC x y
  mulC tupled

dot s0 s1 =
  let products = map2C curried_mul s0 s1 in
  reduceC addC products

dot_prod =
  let s0 = com_input_seq "I0" (Proxy :: Proxy (Seq 840 Atom_UInt16)) in
  let s1 = com_input_seq "I1" (Proxy :: Proxy (Seq 840 Atom_UInt16)) in
  dot s0 s1

dot_prod_throughputs = single_reduce_throughputs

dot_prod_st_prints = sequence $
  fmap (\s -> compile_to_file
              dot_prod (wrap_single_t s)
              text_backend "dot")
  dot_prod_throughputs

dot_prod_chisel_prints = sequence $
  fmap (\s -> compile_to_file
              dot_prod (wrap_single_t s)
              Chisel "dot")
  dot_prod_throughputs

-- 1D convolution
--------------------------------------------------------------------------------

stencil_3_1dC_nested in_seq = do
  let shifted_once = shiftC (Proxy @1) in_seq
  let shifted_twice = shiftC (Proxy @1) shifted_once
  let window_tuple = map2C seq_tuple_appendC
                     (map2C seq_tupleC shifted_twice shifted_once)
                     in_seq
  let partitioned_tuple = partitionC Proxy (Proxy @1) window_tuple
  mapC seq_tuple_to_seqC partitioned_tuple

conv1d =
  let kernel = (list_to_seq (Proxy @3) (fmap Atom_Int8 [-1, 0, 1])) in
  let conv_math xs = dot xs (const_genC kernel xs) in
  let in_seq = com_input_seq "I" (Proxy :: Proxy (Seq 16 Atom_Int8)) in
  let windows = stencil_3_1dC_nested in_seq in
  mapC conv_math windows

conv1d_throughputs = [1 % 3, 1, 2, 4, 8, 16]

conv1d_st_prints = sequence $
  fmap (\s -> compile_to_file
              conv1d (wrap_single_t s)
              text_backend "conv1d")
  conv1d_throughputs

conv1d_chisel_prints = sequence $
  fmap (\s -> compile_to_file
              conv1d (wrap_single_t s)
              Chisel "conv1d")
  conv1d_throughputs
