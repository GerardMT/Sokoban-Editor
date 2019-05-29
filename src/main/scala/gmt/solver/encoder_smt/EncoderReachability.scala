package gmt.solver.encoder_smt

import gmt.collection.SortedMap
import gmt.game.SokobanAction
import gmt.game.SokobanAction.SokobanActionEnum
import gmt.instance.{Coordinate, InstanceSokoban}
import gmt.planner.language
import gmt.planner.language._
import gmt.planner.language.Integer.ImplicitConstructor
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

    class BoxActionReachability(override val direction: Coordinate, instance: InstanceSMT, override val sT: StateReachability, override val sTPlus: StateSMT, override val box: Int) extends BoxActionBasic(direction, instance, sT, sTPlus, box) {

        override protected def preconditions(): immutable.Seq[Term] = {
            val pres = ListBuffer.empty[Term]

            pres.appendAll(super.preconditionWallsBetween())
            pres.appendAll(super.preconditionBoxesBetween())

            for ((c, rn) <- sT.reachabilityNodes.filter(f => instance.existsAndPlayableArea(f._1 + direction))) {
                direction match {
                    case SokobanAction.UP.shift =>
                        pres.append(((sT.boxes(box).x == Integer(c.x)) && (sT.boxes(box).y == Integer(c.y - 1))) ==> rn)

                    case SokobanAction.DOWN.shift =>
                        pres.append(((sT.boxes(box).x == Integer(c.x)) && (sT.boxes(box).y == Integer(c.y + 1))) ==> rn)

                    case SokobanAction.RIGHT.shift =>
                        pres.append(((sT.boxes(box).x == Integer(c.x + 1)) && (sT.boxes(box).y == Integer(c.y))) ==> rn)

                    case SokobanAction.LEFT.shift =>
                        pres.append(((sT.boxes(box).x == Integer(c.x - 1)) && (sT.boxes(box).y == Integer(c.y))) ==> rn)

                    case _ =>
                }
            }

            pres.toList
        }

        override def decode(assignments: immutable.Map[String, Value]): immutable.Seq[SokobanActionEnum] = {
            val actions = ListBuffer.empty[SokobanActionEnum]

            val sTCharacterFrom = sT.coordinateFromCoordinateVariable(sT.character, assignments)
            val sTCharacterTo = sT.coordinateFromCoordinateVariable(sT.boxes(box), assignments) - direction

            if (sTCharacterFrom.manhattanDistance(sTCharacterTo) >= 1) {
                val allNodes = () => instance.map.filter(f => f._2.isPlayableArea).keys
                    .filter(c => !sT.boxes.exists(b => sT.coordinateFromCoordinateVariable(b, assignments) == c))
                    .toList

                val neighbours = (c: Coordinate) => SokobanAction.VALUES.map(a => c + a.shift)
                    .filter(c => instance.existsAndPlayableArea(c))
                    .filter(c => !sT.boxes.exists(b => sT.coordinateFromCoordinateVariable(b, assignments) == c))

                val heuristic = (start: Coordinate, goal: Coordinate) => start.manhattanDistance(goal).toFloat
                val path = AStar.aStar(sTCharacterTo, sTCharacterFrom, allNodes, neighbours, heuristic) match {
                    case Some(p) =>
                        p
                    case None =>
                        throw PlanInvalidException()
                }

                actions.appendAll(path.sliding(2).map(f => SokobanAction.fromShift(f(1) - f(0)).get))
            }

            actions.appendAll(super.decode(assignments))

            actions.toList
        }
    }
}

class EncoderReachability(override val instance: InstanceSokoban) extends EncoderSMT[StateReachability](instance) {

    override def createState(number: Int): StateReachability = new StateReachability(number, instance)

    override def createActions(sT: StateReachability, sTPlus: StateReachability): immutable.Seq[Action[StateReachability, SokobanActionEnum]] = {
        instance.boxes.indices.flatMap(b => SokobanAction.VALUES.map(f => new BoxActionReachability(f.shift, instanceSMT, sT, sTPlus, b).asInstanceOf[Action[StateReachability, SokobanActionEnum]]))
    }

    override def encodeInitialState(state: StateReachability): immutable.Seq[Term] = {
        super.encodeInitialState(state) ++ reachability(state)
    }

    override def encodeOtherStates(state: StateReachability): immutable.Seq[Term] = {
        super.encodeOtherStates(state) ++ reachability(state)
    }

    private def reachability(state: StateReachability): immutable.Seq[Term] = {
        val terms = ListBuffer.empty[Term]

        for ((c, _) <- instance.map.iterator.filter(f => f._2.isPlayableArea)) {
            val ors = for (b <- state.boxes) yield {
                (b.x == Integer(c.x)) && (b.y == Integer(c.y))
            }

            val or = Operations.multipleTermsApply(ors, Or.FUNCTION_MULTIPLE)

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

    override val name: String = "EncoderReachability"
}
