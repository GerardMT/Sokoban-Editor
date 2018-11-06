package gmt.solver.encoder_smt

import gmt.game.SokobanAction
import gmt.game.SokobanAction.SokobanActionEnum
import gmt.planner.language._
import gmt.planner.planner.ClassicPlanner.VariableGenerator
import gmt.planner.solver.value.{Value, ValueInteger}

import scala.collection.immutable
import scala.collection.mutable.ListBuffer

class Repetition(private val preName: String) extends VariableGenerator {

    val repetitionX = Variable(preName + "_PX", Type.Integer)
    val repetitionY = Variable(preName + "_PY", Type.Integer)

    def encode(): immutable.Seq[Term] = {
        val terms = ListBuffer.empty[Term]

        terms.append(ClauseDeclaration((repetitionX == Integer(0)) ^ (repetitionY == Integer(0))))

        terms.toList
    }

    override def getVariables: immutable.Seq[Variable] = List(repetitionX, repetitionY)

    def toSokobanActions(assignmentsMap: immutable.Map[String, Value]): immutable.Seq[SokobanActionEnum] = {
        val valX = assignmentsMap(repetitionX.name).asInstanceOf[ValueInteger].v
        val valY = assignmentsMap(repetitionY.name).asInstanceOf[ValueInteger].v

        val sum = valX + valY

        val positive = sum > 0
        val repetition = Math.abs(sum)
        val horizontal = valX != 0

        val actions = ListBuffer.empty[SokobanActionEnum]

        for (_ <- 0 until repetition) {
            val action = if (horizontal) {
                if (positive) {
                    SokobanAction.RIGHT
                } else {
                    SokobanAction.LEFT
                }
            } else {
                if (positive) {
                    SokobanAction.DOWN
                } else {
                    SokobanAction.UP
                }
            }

            actions.append(action)
        }

        actions.toList
    }
}
