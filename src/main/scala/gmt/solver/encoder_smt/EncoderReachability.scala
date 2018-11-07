package gmt.solver.encoder_smt

import gmt.collection.SortedMap
import gmt.game.SokobanAction
import gmt.game.SokobanAction.SokobanActionEnum
import gmt.instance.{Coordinate, InstanceSokoban}
import gmt.planner.language
import gmt.planner.language._
import gmt.planner.planner.ClassicPlanner._
import gmt.planner.solver.value.{Value, ValueInteger}
import gmt.solver.encoder_smt.EncoderBasic.BoxActionBasic
import gmt.solver.encoder_smt.EncoderReachability.{BoxActionReachability, StateReachability}
import gmt.solver.encoder_smt.EncoderSMT.{InstanceSMT, StateSMT}
import gmt.solver.{CoordinateVariable, PlanInvalidException}
import gmt.util.AStar

import scala.collection.immutable
import scala.collection.mutable.ListBuffer

object EncoderReachability {

    class StateReachability(override val number: Int, private val instance: InstanceSokoban) extends StateSMT(number, instance) {
        val reachabilityNodes: SortedMap[Coordinate, Variable] = SortedMap.empty[Coordinate, Variable]
        val reachabilityWeights: SortedMap[Coordinate, Variable] = SortedMap.empty[Coordinate, Variable]
        val reachabilityEdges: SortedMap[(Coordinate, Coordinate), Variable] = SortedMap.empty[(Coordinate, Coordinate), Variable]

        for ((c, _) <- instance.map.filter(f => f._2.isPlayableArea)) {
            reachabilityNodes.put(c, language.Variable("S" + number + "_RN_X" + c.x + "Y" + c.y, Type.Boolean))
            reachabilityWeights.put(c, Variable("S" + number + "RW_X" + c.x + "Y" + c.y, Type.Integer))

            for (shift <- SokobanAction.VALUES.map(f => f.shift)) {
                val end = c + shift
                if (instance.map.contains(end)) {
                    reachabilityEdges.put((c, end), Variable("S" + number + "_RA_X" + c.x + "Y" + c.y + "TX" + end.x + "Y" + end.y, Type.Boolean))
                }
            }
        }

        override def getVariables: immutable.Seq[Variable] = {
            val variables = ListBuffer(super.getVariables: _*)

            variables.appendAll(reachabilityNodes.values)
            variables.appendAll(reachabilityWeights.values)
            variables.appendAll(reachabilityEdges.values)

            variables.toList
        }
    }

    class BoxActionReachability(instance: InstanceSMT, override val sT: StateReachability, override val sTPlus: StateSMT, override val box: Int) extends BoxActionBasic(instance, sT, sTPlus, box) with RepetitionInterface {

        override protected def preconditions(): immutable.Seq[Term] = {
            val ands = ListBuffer.empty[Term]

            ands.appendAll(super.preconditions())

            for ((c, rn) <- sT.reachabilityNodes) {
                ands.append((sT.character.x == Integer(c.x) && sT.character.y == Integer(c.y)) ==> rn)
            }

            ands.toList
        }

        override def decode(assignments: immutable.Map[String, Value]): immutable.Seq[SokobanActionEnum] = {
            val actions = ListBuffer.empty[SokobanActionEnum]

            val sTCharacter = coordinateFromCoordinateVariable(sT.character, assignments)
            val sTPlusCharacter = coordinateFromCoordinateVariable(sTPlus.character, assignments)

            if (sTCharacter.manhattanDistance(sTPlusCharacter) > 1) {
                val allNodes = () => instance.map.filter(f => f._2.isPlayableArea).keys.toList

                val neighbours = (c: Coordinate) => SokobanAction.VALUES.map(a => c + a.shift).filter(c => instance.existsAndPlayableArea(c))
                val heuristic = (start: Coordinate, goal: Coordinate) => start.euclideanDistance(goal).toFloat

                val path = AStar.aStar(sTPlusCharacter, sTCharacter, allNodes, neighbours, heuristic) match {
                    case Some(p) =>
                        p
                    case None =>
                        throw PlanInvalidException()
                }

                actions.appendAll(path.map(f => SokobanAction.fromShift(f).get))
            }

            actions.appendAll(repetition.toSokobanActions(assignments))

            actions.toList
        }

        private def coordinateFromCoordinateVariable(coordinateVariables: CoordinateVariable, assignmentsMap: immutable.Map[String, Value]): Coordinate = {
            Coordinate(assignmentsMap(coordinateVariables.x.name).asInstanceOf[ValueInteger].v, assignmentsMap(coordinateVariables.y.name).asInstanceOf[ValueInteger].v)
        }
    }
}

class EncoderReachability(override val instance: InstanceSokoban) extends EncoderSMT[StateReachability](instance) {

    override def createState(number: Int): StateReachability = {
        new StateReachability(number, instance)
    }

    override def createActions(sT: StateReachability, sTPlus: StateReachability): immutable.Seq[Action[StateReachability, SokobanActionEnum]] = {
        instance.boxes.indices.map(b => new BoxActionReachability(instanceSMT, sT, sTPlus, b).asInstanceOf[Action[StateReachability, SokobanActionEnum]])
    }

    override def encodeInitialState(state: StateReachability): immutable.Seq[Term] = {
        reachability(state)
    }

    override def encodeTimeStep(timeStep: TimeStep[StateReachability, SokobanActionEnum]): immutable.Seq[Term] = {
        reachability(timeStep.sTPlus)
    }

    private def reachability(state: StateReachability): immutable.Seq[Term] = {
        val terms = ListBuffer.empty[Term]

        for ((c, _) <- instance.map.iterator.filter(f => f._2.isPlayableArea)) {
            val or = Or((for (b <- instance.boxes.indices) yield {
                (state.boxes(b).x == Integer(c.x)) && (state.boxes(b).y == Integer(c.y))
            }): _*)

            terms.append(ClauseDeclaration(or ==> !state.reachabilityNodes.get(c).get))
        }

        for ((c, _) <- instance.map.filter(f => f._2.isPlayableArea)) {
            val nodeStart = state.reachabilityNodes.get(c).get

            val ors = ListBuffer.empty[Term]


            for (end <- SokobanAction.VALUES.map(f => c + f.shift).filter(f => instance.existsAndPlayableArea(f))) {
                val edgeInverse = state.reachabilityEdges.get((end, c)).get

                ors.append(edgeInverse)

                terms.append(ClauseDeclaration(edgeInverse ==> state.reachabilityNodes.get(end).get))
                terms.append(ClauseDeclaration(edgeInverse ==> (state.reachabilityWeights.get(end).get < state.reachabilityWeights.get(c).get)))
            }

            if (ors.nonEmpty) {
                terms.append(ClauseDeclaration(((!(state.character.x == Integer(c.x)) || !(state.character.y == Integer(c.y))) && nodeStart) ==> Operations.multipleTermsApply(ors.toList, Or.FUNCTION_MULTIPLE)))
            }
        }

        terms.toList
    }
}
