package gmt.solver

import gmt.game.SokobanAction.SokobanActionEnum
import gmt.instance.InstanceSokoban
import gmt.planner.planner.ClassicPlanner
import gmt.planner.planner.ClassicPlanner.{ClassicPlannerUpdatesCallback, State}

abstract class EncoderSokoban[S <: State](val instance: InstanceSokoban, override val updatesCallback: ClassicPlannerUpdatesCallback) extends ClassicPlanner[S, InstanceSokoban, SokobanActionEnum](updatesCallback) {
    val name: String
}
