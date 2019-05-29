package gmt.solver

import gmt.instance.InstanceSokoban
import gmt.planner.planner.ClassicPlanner.State
import gmt.solver.encoder_smt.{EncoderBasic, EncoderReachability}

object EncoderFactory {

    def apply(instance: InstanceSokoban): Seq[EncoderSokoban[_ <: State]] = {
        Seq(new EncoderBasic(instance), new EncoderReachability(instance))
    }
}
