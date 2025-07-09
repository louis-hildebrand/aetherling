module Test_More_Simple_Benchmarks where
import Aetherling.Interpretations.Backend_Execute.Compile
import Aetherling.Languages.Sequence.Shallow.Expr
import Aetherling.Languages.Sequence.Shallow.Types
import Aetherling.Rewrites.Sequence_Shallow_To_Deep
import Data.Proxy
import Data.Ratio

single_reduce_sum =
  reduceC' (Proxy @200) addC $
  com_input_seq "I" (Proxy :: Proxy (Seq 200 Atom_UInt8))

single_reduce_throughputs = map (\t -> t % 200) [1,2,4,5,8,10,20,40,200]

single_reduce_sum_st_prints = sequence $
  fmap (\s -> compile_to_file
              single_reduce_sum (wrap_single_t s)
              text_backend "reduce")
  single_reduce_throughputs

single_reduce_sum_chisel_prints = sequence $
  fmap (\s -> compile_to_file
              single_reduce_sum (wrap_single_t s)
              Chisel "reduce")
  single_reduce_throughputs
