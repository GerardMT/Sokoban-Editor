package gmt.solver

import gmt.collection.SortedMap
import gmt.game.SokobanAction
import gmt.instance.{Coordinate, Instance}
import gmt.planner.encoder.{Encoder, EncoderResult, Encoding}
import gmt.planner.operation
import gmt.planner.operation.{And, ClauseDeclaration, Comment, Integer, Operations, Or, Term, Type, Variable, VariableDeclaration}
import gmt.planner.solver.Assignment
import gmt.solver.SokobanEncoder.{SokobanEncodingResult, State}

import scala.collection.immutable
import scala.collection.mutable.ListBuffer

object SokobanEncoder {

    case class SokobanEncodingResult(states: immutable.Seq[State])

    case class TupleCoordinates(x: Variable, y: Variable)

    class State (private val instance: Instance, val number: Int) {

        val character: TupleCoordinates = TupleCoordinates(Variable("S" + number + "_C_X", Type.Integer), Variable("S" + number + "_C_Y", Type.Integer))
        val boxes: immutable.Seq[TupleCoordinates] = instance.boxes.indices
            .map(i => TupleCoordinates(Variable("S" + number + "_B_" + i + "_X", Type.Integer), Variable("S" + number + "_B_" + i + "Y", Type.Integer))).toVector

        val reachabilityNodes: SortedMap[Coordinate, Variable] = SortedMap.empty[Coordinate, Variable]
        val reachabilityWeights: SortedMap[Coordinate, Variable] = SortedMap.empty[Coordinate, Variable]
        val reachabilityEdges: SortedMap[(Coordinate, Coordinate), Variable] = SortedMap.empty[(Coordinate, Coordinate), Variable]

        for ((c, l) <- instance.map.filter(f => f._2.isPlayableArea)) {
            reachabilityNodes.put(c, operation.Variable("S" + number + "_RN_X" + c.x + "Y" + c.y, Type.Boolean))
            reachabilityWeights.put(c, Variable("S" + number + "RW_X" + c.x + "Y" + c.y, Type.Integer))

            for (shift <- SokobanAction.ACTIONS.map(f => f.shift)) {
                val end = c + shift
                if (instance.map.contains(end)) {
                    reachabilityEdges.put((c, end), Variable("S" + number + "_RA_X" + c.x + "Y" + c.y + "TX" + end.x + "Y" + end.y, Type.Boolean))
                }
            }
        }
        def addVariables(encoding: Encoding): Unit = {
            encoding.add(VariableDeclaration(character.x))
            encoding.add(VariableDeclaration(character.y))

            for (b <- boxes) {
                encoding.add(VariableDeclaration(b.x))
                encoding.add(VariableDeclaration(b.y))
            }
        }
    }
}

class SokobanEncoder[A](private val instance: Instance) extends Encoder[A, SokobanEncodingResult] {

    override def encode(timeSteps: Int): EncoderResult[SokobanEncodingResult] = {
        val encoding = new Encoding()

        val states = (0 to timeSteps).map(f => new State(instance, f)).toVector

        var s0 = new State(instance, 0)

        encoding.add(reachability(s0): _*)

        for ((sT, sTPlus) <- states.zip(states.tail)) {
            encoding.add(reachability(sTPlus): _*)

            val actionsVariables = ListBuffer.empty[Variable]

            for ((b, name) <- instance.boxes.indices.map(f => (f, f.toString))) {
                val actionVariable = Variable(prefixAction(sT, sTPlus, name), Type.Integer)
                encoding.add(VariableDeclaration(actionVariable))
                actionsVariables.append(actionVariable)

                val ActionEncoderResult(pre, eff, terms) = boxAction(Frame(sT, sTPlus), b, name)
                encoding.add(terms: _*)

                encoding.add(ClauseDeclaration(eff == actionVariable))
                encoding.add(ClauseDeclaration(actionVariable -> pre))
            }

            encoding.add(Operations.eoWithQuatradicAmmo(actionsVariables): _*)
        }

        encoding.add(goal(states.last): _*)

        EncoderResult(encoding, SokobanEncodingResult())
    }

    private def reachability(state: State): immutable.Seq[Term] = {
        val terms = ListBuffer.empty[Term]

        terms.append(Comment("Reachability"))
        for ((c, l) <- instance.map.iterator.filter(f => f._2.isPlayableArea)) {
            val or = Or((for (b <- instance.boxes.indices) yield {
                (state.boxes(b).x == Integer(c.x)) && (state.boxes(b).y == Integer(c.y))
            }): _*)

            terms.append(ClauseDeclaration(or -> !state.reachabilityNodes.get(c).get))
        }

        for ((c, l) <- instance.map.filter(f => f._2.isPlayableArea)) {
            val nodeStart = state.reachabilityNodes.get(c).get

            val ors = ListBuffer.empty[Term]


            for (end <- SokobanAction.ACTIONS.map(f => c + f.shift).filter(f => existsAndPlayableArea(instance, f))) {
                val edgeInverse = state.reachabilityEdges.get((end, c)).get

                ors.append(edgeInverse)

                terms.append(ClauseDeclaration(edgeInverse -> state.reachabilityNodes.get(end).get))
                terms.append(ClauseDeclaration(edgeInverse -> (state.reachabilityWeights.get(end).get < state.reachabilityWeights.get(c).get)))
            }

            if (ors.nonEmpty) {
                terms.append(ClauseDeclaration(((!(state.character.x == Integer(c.x)) || !(state.character.y == Integer(c.y))) && nodeStart) -> Operations.simplify(Or(ors: _*))))
            }
        }

        terms.toList
    }

    private def boxAction(frame: Frame, aB: Int, name: String): ActionEncoderResult = {
        val Frame(sT, sTPlus) = frame

        val terms = ListBuffer.empty[Term]

        val repetitionX = Variable(prefixAction(sT, sTPlus, name), Type.Integer)
        val repetitionY = Variable(prefixAction(sT, sTPlus, name), Type.Integer)

        terms.append(ClauseDeclaration((repetitionX == Integer(0)) || repetitionY == Integer(0)))

        val ands = ListBuffer.empty[Term]

        for (b <- sT.boxes) {
            ands.append((b.x < sT.boxes(aB).x) || (b.x > (sT.boxes(aB).x + repetitionX)))
            ands.append((b.y < sT.boxes(aB).y) || (b.y > (sT.boxes(aB).y + repetitionY)))
        }
        for ((c, _) <- instance.map.filter(f => !f._2.isPlayableArea)) {
            ands.append((Integer(c.x) < sT.boxes(aB).x) || (Integer(c.x) > (sT.boxes(aB).x + repetitionX)))
            ands.append((Integer(c.y) < sT.boxes(aB).y) || (Integer(c.y) > (sT.boxes(aB).y + repetitionY)))
        }

        val pre = And(ands: _*)
        val eff = (sTPlus.boxes(aB).x == sT.boxes(aB).x) && (sTPlus.boxes(aB).y == sT.boxes(aB).y)

        ActionEncoderResult(pre, eff, terms.toList)
    }

    private def goal(state: State): immutable.Seq[Term] = {
        val terms = ListBuffer.empty[Term]

        for (b <- state.boxes) {
            val or = Or((for (g <- instance.goals) yield {
                ClauseDeclaration((b.x == Integer(g.x)) && (b.y == Integer(g.y)))
            }): _*)
            terms.append(ClauseDeclaration(or))
        }

        terms.toList
    }

    override def decode(assignments: Seq[Assignment], encodingData: SokobanEncodingResult): A = {

    }

    override def lowerBound(): Int = {
        throw UnsupportedOperationException
    }

    override def upperBound(): Int = {
        throw UnsupportedOperationException
    }

    private def existsAndPlayableArea(instance: Instance, coordinate: Coordinate): Boolean = {
        instance.map.get(coordinate) match {
            case Some(o) =>
                o.isPlayableArea
            case None =>
                false
        }
    }

    private def prefixAction(sT: State, sTPlus: State, actionName: String): String = {
        "S" + sT.number + "_S" + sTPlus.number  + "A_" + actionName
    }

    private case class ActionEncoderResult(pre: Term, post: Term, terms: immutable.Seq[Term])

    private case class Frame(sT: State, sTPlus: State)

    private case class Area(c: Coordinate, width: Int, height: Int)

    private case class WallAreas(areas: immutable.Seq[Area], walls: immutable.Seq[Coordinate])
}
