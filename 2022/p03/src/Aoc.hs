module Aoc where

import Clash.Explicit.Testbench (outputVerifier', stimuliGenerator, tbClockGen)
import Clash.Prelude (BitVector, Bool, Clock, Enable, Reset, Signal, System, Unsigned, enableGen, exposeClockResetEnable, mealy, not, replaceBit, resetGen, (<$>))
import Clash.Sized.Vector (Vec (Nil, (:>)))

bvHistT :: BitVector 8 -> Unsigned 3 -> (BitVector 8, BitVector 8)
bvHistT bv x =
  let bv' = replaceBit x 1 bv
      bv'' = replaceBit x 1 bv'
   in (bv'', bv'')

topEntity :: Clock System -> Reset System -> Enable System -> Signal System (Unsigned 3) -> Signal System (BitVector 8)
topEntity =
  exposeClockResetEnable (mealy bvHistT 0)

testBench :: Signal System Bool
testBench =
  let en = enableGen
      clk = tbClockGen (not <$> done)
      rst = resetGen
      inputSignal = stimuliGenerator clk rst (5 :> 4 :> 0 :> 1 :> 2 :> Nil)
      expectOutput =
        outputVerifier'
          clk
          rst
          (32 :> 48 :> 49 :> 51 :> 55 :> Nil)
      done = expectOutput (topEntity clk rst en inputSignal)
   in done
