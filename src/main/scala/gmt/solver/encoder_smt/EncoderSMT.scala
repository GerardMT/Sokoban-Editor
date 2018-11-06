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

    class InstanceSMT(private var sokoban: InstanceSokoban, bounds: Bounds) extends InstanceSokoban(sokoban)

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

    abstract class ActionSMT(val instance: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT) extends Action[StateSMT, SokobanActionEnum](sT, sTPlus)

    def preconditionBounds[A <: ActionSMT with Repetition](action: A): immutable.Seq[Term] = {
        val ands = ListBuffer.empty[Term]

        for ((c, _) <- action.instance.map.filter(f => !f._2.isPlayableArea)) {
            ands.append(Ite(action.repetitionX < Integer(0) || action.repetitionY < Integer(0),
                ((Integer(c.x) < action.sT.character.x) || (Integer(c.x) > (action.sT.character.x + action.repetitionX + Integer(1)))) && ((Integer(c.y) < action.sT.character.y) || (Integer(c.y) > (action.sT.character.y + action.repetitionY + Integer(1)))),
                ((Integer(c.x) > action.sT.character.x) || (Integer(c.x) < (action.sT.character.x - action.repetitionX - Integer(1)))) && ((Integer(c.y) > action.sT.character.y) || (Integer(c.y) < (action.sT.character.y - action.repetitionY - Integer(1))))))
        }

        ands.toList
    }
}

abstract class EncoderSMT[S <: StateSMT](val instance: InstanceSokoban) extends ClassicPlanner[S, InstanceSokoban, SokobanActionEnum] {

    protected val instanceSMT = new InstanceSMT(instance, getBounds)

    override def goal(state: S): immutable.Seq[Term] = {
        val terms = ListBuffer.empty[Term]

        for (b <- state.boxes) {
            val or = Or((for (g <- instance.goals) yield {
                (b.x == Integer(g.x)) && (b.y == Integer(g.y))
            }): _*)
            terms.append(ClauseDeclaration(or))
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
