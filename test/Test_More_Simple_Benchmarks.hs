module Test_More_Simple_Benchmarks where
import Aetherling.Interpretations.Backend_Execute.Compile
import Aetherling.Languages.Sequence.Shallow.Expr
import Aetherling.Languages.Sequence.Shallow.Types
import Aetherling.Rewrites.Sequence_Shallow_To_Deep
import Data.Proxy
import Data.Ratio

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

curried_mul x y = do
  let tupled = atom_tupleC x y
  mulC tupled

dot_prod =
  let s0 = com_input_seq "I0" (Proxy :: Proxy (Seq 840 Atom_UInt16)) in
  let s1 = com_input_seq "I1" (Proxy :: Proxy (Seq 840 Atom_UInt16)) in
  let products = map2C curried_mul s0 s1 in
  reduceC addC products

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
