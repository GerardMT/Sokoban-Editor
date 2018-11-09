package gmt.solver.encoder_smt

import gmt.game.SokobanAction
import gmt.instance.Coordinate
import gmt.planner.language._
import gmt.planner.planner.ClassicPlanner.VariableGenerator
import gmt.planner.solver.value.{Value, ValueInteger}

import scala.collection.immutable

class Repetition(private val preName: String) extends VariableGenerator {

    val repetition = Variable(preName + "_R", Type.Integer)

    override def getVariables: immutable.Seq[Variable] = List(repetition)

    def decode(assignmentsMap: immutable.Map[String, Value]): Int = {
        assignmentsMap(repetition.name).asInstanceOf[ValueInteger].v
    }
}
