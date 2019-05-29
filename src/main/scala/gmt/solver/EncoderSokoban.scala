package gmt.solver

import gmt.game.SokobanAction.SokobanActionEnum
import gmt.instance.InstanceSokoban
import gmt.planner.planner.ClassicPlanner
import gmt.planner.planner.ClassicPlanner.State

abstract class EncoderSokoban[S <: State](val instance: InstanceSokoban) extends ClassicPlanner[S, InstanceSokoban, SokobanActionEnum] {
    val name: String
}
