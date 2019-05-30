package gmt.solver.encoder_smt

import gmt.game.GameObject.{ALIGN, BOX, CHARACTER, EMPTY, WALL}
import gmt.game.SokobanAction.SokobanActionEnum
import gmt.instance.{Coordinate, InstanceSokoban}
import gmt.planner.language._
import gmt.planner.planner.ClassicPlanner
import gmt.planner.planner.ClassicPlanner.{Action, ClassicPlannerUpdatesCallback, State}
import gmt.planner.solver.value.{Value, ValueInteger}
import gmt.solver.encoder_smt.EncoderSMT.{Bounds, InstanceSMT, StateSMT}
import gmt.solver.{CoordinateVariable, EncoderSokoban}

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer

object EncoderSMT {

    case class Bounds(min: Coordinate, max: Coordinate, walls: immutable.Seq[Coordinate])

    class InstanceSMT(private var sokoban: InstanceSokoban, val bounds: Bounds) extends InstanceSokoban(sokoban)

    class StateSMT(override val number: Int, val instance: InstanceSokoban) extends State(number) {
        val character: CoordinateVariable = CoordinateVariable(Variable("S" + number + "_C_X", Type.Integer), Variable("S" + number + "_C_Y", Type.Integer))
        val boxes: immutable.Seq[CoordinateVariable] = instance.boxes.indices
            .map(i => CoordinateVariable(Variable("S" + number + "_B_" + i + "_X", Type.Integer), Variable("S" + number + "_B_" + i + "_Y", Type.Integer))).toVector

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

        def coordinateFromCoordinateVariable(coordinateVariables: CoordinateVariable, assignmentsMap: immutable.Map[String, Value]): Coordinate = {
            Coordinate(assignmentsMap(coordinateVariables.x.name).asInstanceOf[ValueInteger].v, assignmentsMap(coordinateVariables.y.name).asInstanceOf[ValueInteger].v)
        }

        override def decode(assignments: Map[String, Value], updatesCallback: ClassicPlannerUpdatesCallback): Unit = {

            updatesCallback.stateDecoded match {
                case Some(f: Function2[StateSMT, Map[String, Value], Unit]) => f(this, assignments)
                case None =>
            }
        }
    }

    abstract class ActionSMT(val instanceSMT: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT) extends Action[StateSMT, SokobanActionEnum](sT, sTPlus)
}

abstract class EncoderSMT[S <: StateSMT](override val instance: InstanceSokoban, override val updatesCallback: ClassicPlannerUpdatesCallback) extends EncoderSokoban[S](instance, updatesCallback) {

    protected val instanceSMT = new InstanceSMT(instance, getBounds)

    def encodeOtherStates(timeStep: ClassicPlanner.TimeStep[S, SokobanActionEnum]): immutable.Seq[Term] = {
        List(ClauseDeclaration(timeStep.sT.character.x > Integer(instanceSMT.bounds.min.x)),
            ClauseDeclaration(timeStep.sT.character.y > Integer(instanceSMT.bounds.min.y)),
            ClauseDeclaration(timeStep.sT.character.x < Integer(instanceSMT.bounds.max.x)),
            ClauseDeclaration(timeStep.sT.character.y < Integer(instanceSMT.bounds.max.y)))
    }


    def encodeInitialState(state: S): immutable.Seq[Term] = {
        val terms = ListBuffer.empty[Term]

        terms.append(ClauseDeclaration(state.character.x == Integer(instance.character.x)))
        terms.append(ClauseDeclaration(state.character.y == Integer(instance.character.y)))

        for (b <- instance.boxes.indices) {
            terms.append(ClauseDeclaration(state.boxes(b).x == Integer(instance.boxes(b).x)))
            terms.append(ClauseDeclaration(state.boxes(b).y == Integer(instance.boxes(b).y)))
        }

        terms.toList
    }

    def encodeGoal(state: S): immutable.Seq[Term] = {
        val terms = ListBuffer.empty[Term]

        for (b <- state.boxes) {
            val or = for (g <- instance.goals) yield {
                (b.x == Integer(g.x)) && (b.y == Integer(g.y))
            }

            terms.append(ClauseDeclaration(Operations.multipleTermsApply(or, Or.FUNCTION_MULTIPLE)))
        }

        terms.toList
    }

    def lowerBound(): Int = {
        throw new UnsupportedOperationException()
    }

    def upperBound(): Int = {
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
