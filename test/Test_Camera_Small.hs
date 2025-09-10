module Test_Camera_Small where
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test_Slowdown as TS
import Aetherling.Monad_Helpers
import Aetherling.Languages.Sequence.Shallow.Expr
import Aetherling.Languages.Sequence.Shallow.Types
import Aetherling.Languages.Sequence.Deep.Expr
import Aetherling.Languages.Sequence.Deep.Types
import Aetherling.Languages.Isomorphisms
import Aetherling.Interpretations.Compute_Latency
import Aetherling.Interpretations.Backend_Execute.Test_Helpers
import qualified Aetherling.Languages.Space_Time.Deep.Expr as STE
import qualified Aetherling.Languages.Space_Time.Deep.Types as STT
import Aetherling.Rewrites.Sequence_Shallow_To_Deep
import Aetherling.Rewrites.Rewrite_Helpers
import Aetherling.Rewrites.Sequence_To_Partially_Parallel_Space_Time.Rewrite_Expr
import Aetherling.Rewrites.Sequence_To_Partially_Parallel_Space_Time.Rewrite_All_Types
import Aetherling.Rewrites.Sequence_Assign_Indexes
import Aetherling.Languages.Space_Time.Deep.Type_Checker
import Aetherling.Interpretations.Backend_Execute.Compile
import Control.Applicative
import Data.Proxy
import Data.Traversable
import GHC.TypeLits
import GHC.TypeLits.Extra
import Data.List
import Data.Ratio
import Data.Word
import qualified Test_Demosaic_Small as TD
import qualified Test_Small_Real_Math32 as TSR

row_size_camera :: Integer = 8
col_size_camera :: Integer = 8
img_size_camera :: Int = fromInteger $ col_size_camera*row_size_camera
small_camera = do
  let demosaic_out = TD.demosaic_test $
        com_input_seq "I" (Proxy :: Proxy (Seq 64 Atom_UInt32))
  let red = TSR.sharpen_shallow_no_input (Proxy @8) $ mapC fstC demosaic_out
  let green = TSR.sharpen_shallow_no_input (Proxy @8) $ mapC (fstC . sndC) demosaic_out
  let blue = TSR.sharpen_shallow_no_input (Proxy @8) $ mapC (sndC . sndC) demosaic_out
  map2C atom_tupleC red $ map2C atom_tupleC green blue
camera_throughputs = [2, 1, 1 % 4]
small_camera_inputs :: [[Word32]] = map (map fromIntegral) [[i * i | i <- [1..row_size_camera * col_size_camera]]]
small_camera_output :: [(Word32, (Word32, Word32))] = do
  let demosaic_out = demosaic_generator TD.row_size_demosaic
                     (small_camera_inputs !! 0)
  let red_demosaic = map fst demosaic_out
  let red = zipWith sharpen_one_pixel'
             (conv_generator $ stencil_generator row_size_camera $ red_demosaic)
             red_demosaic
  let green_demosaic = map (fst . snd) demosaic_out
  let green = zipWith sharpen_one_pixel'
             (conv_generator $ stencil_generator row_size_camera $ green_demosaic)
             green_demosaic
  let blue_demosaic = map (snd . snd) demosaic_out
  let blue = zipWith sharpen_one_pixel'
             (conv_generator $ stencil_generator row_size_camera $ blue_demosaic)
             blue_demosaic
  zip red $ zip green blue

small_camera_st_prints = sequence $
  fmap (\s -> compile_to_file
              small_camera (wrap_single_t s)
              text_backend "smallcamera") camera_throughputs
small_camera_chisel_prints = sequence $
  fmap (\s -> compile_to_file
              small_camera (wrap_single_t s)
              Chisel "smallcamera") camera_throughputs

dump_small_camera_outputs :: IO ()
dump_small_camera_outputs = do
  let str = intercalate "\n" $
            map (\(r, (g, b)) -> intercalate "," $ map show [r, g, b]) $
            take ((fromInteger row_size_camera) * 8) small_camera_output
  writeFile "test/no_bench/smallcamera_outputs.csv" str
