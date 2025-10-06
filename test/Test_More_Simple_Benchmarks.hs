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
  let s0 = com_input_seq "I0" (Proxy :: Proxy (Seq 512 Atom_UInt16)) in
  let s1 = com_input_seq "I1" (Proxy :: Proxy (Seq 512 Atom_UInt16)) in
  dot s0 s1

dot_prod_throughputs = map (\t -> t % 512) [1, 2, 4, 8, 16]

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

-- Matrix-vector multiplication
--------------------------------------------------------------------------------

small_mvm =
  let mat = com_input_seq "I0" (Proxy :: Proxy (Seq 16 Atom_UInt8)) in
  let vec = com_input_seq "I1" (Proxy :: Proxy (Seq 4 Atom_UInt8)) in
  mapC (\row -> dot row vec) (partitionC (Proxy @4) (Proxy @4) mat)

small_mvm_throughputs = [1%4]

small_mvm_st_prints = sequence $
  fmap (\s -> compile_to_file
              small_mvm (wrap_single_t s)
              text_backend "smallmvm")
  small_mvm_throughputs

small_mvm_chisel_prints = sequence $
  fmap (\s -> compile_to_file
              small_mvm (wrap_single_t s)
              Chisel "smallmvm")
  small_mvm_throughputs

-- Integer square root
--------------------------------------------------------------------------------

my_sqrt in_seq = do
  let init n = let lo = const_genC (Atom_UInt16 0) n in
               let hi = const_genC (Atom_UInt16 255) n in
               atom_tupleC n (atom_tupleC lo hi)
  let step x = let n = fstC x in
               let lo = fstC $ sndC x in
               let hi = sndC $ sndC x in
               let one8 = const_genC (Atom_UInt8 1) n in
               let one16 = const_genC (Atom_UInt16 1) n in
               let lo_plus_hi_plus_one = addC $ atom_tupleC one16 (addC $ atom_tupleC lo hi) in
               let mid = lsrC $ atom_tupleC lo_plus_hi_plus_one one8 in
               let mid_sq = mulC $ atom_tupleC mid mid in
               let c = ltC $ atom_tupleC n mid_sq in
               let out_then = atom_tupleC lo (subC $ atom_tupleC mid one16) in
               let out_else = atom_tupleC mid hi in
               let new_lo_hi = ifC $ atom_tupleC c $ atom_tupleC out_then out_else in
               atom_tupleC n new_lo_hi
  let n_lo_hi_0 = mapC init in_seq
  let n_lo_hi_16 = foldr (\_ -> mapC step) n_lo_hi_0 [1..16]
  mapC (\x -> fstC $ sndC x) n_lo_hi_16

sqrt_bench =
  let s = com_input_seq "I" (Proxy :: Proxy (Seq 1024 Atom_UInt16)) in
  my_sqrt s

sqrt_throughputs = [1, 4]

sqrt_st_prints = sequence $
  fmap (\s -> compile_to_file
              sqrt_bench (wrap_single_t s)
              text_backend "sqrt")
  sqrt_throughputs

sqrt_chisel_prints = sequence $
  fmap (\s -> compile_to_file
              sqrt_bench (wrap_single_t s)
              Chisel "sqrt")
  sqrt_throughputs
