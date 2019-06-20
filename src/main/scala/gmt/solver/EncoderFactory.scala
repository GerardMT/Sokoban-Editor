package gmt.solver

import gmt.instance.InstanceSokoban
import gmt.planner.planner.ClassicPlanner.{ClassicPlannerUpdatesCallback, State}
import gmt.solver.encoder_smt.{EncoderBasic, EncoderReachabilityFolding}

object EncoderFactory {

    def apply(instance: InstanceSokoban, updatesCallback: ClassicPlannerUpdatesCallback): Seq[EncoderSokoban[_ <: State]] = {
        Seq(new EncoderBasic(instance, updatesCallback), new EncoderReachabilityFolding(instance, updatesCallback))
    }
}
