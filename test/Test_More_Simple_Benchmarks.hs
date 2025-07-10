module Test_More_Simple_Benchmarks where
import Aetherling.Interpretations.Backend_Execute.Compile
import Aetherling.Languages.Sequence.Shallow.Expr
import Aetherling.Languages.Sequence.Shallow.Types
import Aetherling.Rewrites.Sequence_Shallow_To_Deep
import Data.Proxy
import Data.Ratio

single_reduce_sum =
  reduceC' (Proxy @840) addC $
  com_input_seq "I" (Proxy :: Proxy (Seq 840 Atom_UInt8))

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
