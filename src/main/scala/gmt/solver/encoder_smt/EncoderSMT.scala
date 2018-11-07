package gmt.solver.encoder_smt

import gmt.game.GameObject.{ALIGN, WALL}
import gmt.game.SokobanAction.SokobanActionEnum
import gmt.instance.{Coordinate, InstanceSokoban}
import gmt.planner.language._
import gmt.planner.planner.ClassicPlanner
import gmt.planner.planner.ClassicPlanner.{Action, State}
import gmt.solver.CoordinateVariable
import gmt.solver.encoder_smt.EncoderSMT.{Bounds, InstanceSMT, StateSMT}

import scala.collection.immutable
import scala.collection.mutable.ListBuffer

object EncoderSMT {

    case class Bounds(min: Coordinate, max: Coordinate, walls: immutable.Seq[Coordinate])

    class InstanceSMT(private var sokoban: InstanceSokoban, val bounds: Bounds) extends InstanceSokoban(sokoban)

    class StateSMT(override val number: Int, instance: InstanceSokoban) extends State(number) {
        val character: CoordinateVariable = CoordinateVariable(Variable("S" + number + "_C_X", Type.Integer), Variable("S" + number + "_C_Y", Type.Integer))
        val boxes: immutable.Seq[CoordinateVariable] = instance.boxes.indices
            .map(i => CoordinateVariable(Variable("S" + number + "_B_" + i + "_X", Type.Integer), Variable("S" + number + "_B_" + i + "Y", Type.Integer))).toVector

        override def getVariables: immutable.Seq[Variable] = {
            val variables = ListBuffer.empty[Variable]

            variables.append(character.x)
            variables.append(character.y)

            for (b <- boxes) {
                variables.append(b.x)
                variables.append(b.y)
            }

            variables.toList
        }
    }

    abstract class ActionSMT(val instanceSMT: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT) extends Action[StateSMT, SokobanActionEnum](sT, sTPlus)
}

abstract class EncoderSMT[S <: StateSMT](val instance: InstanceSokoban) extends ClassicPlanner[S, InstanceSokoban, SokobanActionEnum] {

    protected val instanceSMT = new InstanceSMT(instance, getBounds)

    override def encodeTimeStep(timeStep: ClassicPlanner.TimeStep[S, SokobanActionEnum]): immutable.Seq[Term] = {
        List(timeStep.sT.character.x > Integer(instanceSMT.bounds.min.x),
            timeStep.sT.character.y > Integer(instanceSMT.bounds.min.y),
            timeStep.sT.character.x < Integer(instanceSMT.bounds.max.x),
            timeStep.sT.character.y < Integer(instanceSMT.bounds.max.y))
    }


    override def encodeInitialState(state: S): immutable.Seq[Term] = {
        val terms = ListBuffer.empty[Term]

        terms.append(ClauseDeclaration(state.character.x == Integer(instance.character.x)))
        terms.append(ClauseDeclaration(state.character.y == Integer(instance.character.y)))

        for (b <- instance.boxes.indices) {
            terms.append(ClauseDeclaration(state.boxes(b).x == Integer(instance.boxes(b).x)))
            terms.append(ClauseDeclaration(state.boxes(b).y == Integer(instance.boxes(b).y)))
        }

        terms.toList
    }

    override def encodeGoal(state: S): immutable.Seq[Term] = {
        val terms = ListBuffer.empty[Term]

        for (b <- state.boxes) {
            val or = for (g <- instance.goals) yield {
                (b.x == Integer(g.x)) && (b.y == Integer(g.y))
            }

            terms.append(ClauseDeclaration(Operations.multipleTermsApply(or, Or.FUNCTION_MULTIPLE)))
        }

        terms.toList
    }

    override def lowerBound(): Int = {
        throw new UnsupportedOperationException()
    }

    override def upperBound(): Int = {
        throw new UnsupportedOperationException()
    }

    private def getBounds: Bounds = {
        val map = Array.ofDim[Char](instance.width, instance.height)

        for (i <- 0 until instance.width) {
            for (j <- 0 until instance.height) {
                map(i)(j) = WALL.char
            }
        }

        for ((c, o) <- instance.map) {
            map(c.x)(c.y) = o.char
        }

        val walls = ListBuffer.empty[Coordinate]

        for (i <- 0 until instance.width) {
            for (j <- 0 until instance.height) {
                if (map(i)(j) == WALL.char) {
                    walls.append(Coordinate(i, j))
                }
            }
        }

        Bounds(Coordinate(0, 0), Coordinate(instance.width, instance.height), walls.toList)
    }
}
