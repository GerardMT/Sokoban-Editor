package gmt.solver.encoder_smt

import gmt.collection.SortedMap
import gmt.game.SokobanAction
import gmt.game.SokobanAction.SokobanActionEnum
import gmt.instance.{Coordinate, Instance}
import gmt.planner.language
import gmt.planner.language.{And, ClauseDeclaration, Integer, Operations, Or, Term, Type, Variable}
import gmt.planner.solver.Assignment
import gmt.planner.solver.value.{Value, ValueInteger}
import gmt.solver.encoder_smt.EncoderBase._
import gmt.solver.encoder_smt.EncoderReachability._
import gmt.solver.{PlanInvalidException, SokobanPlan}
import gmt.util.AStar

import scala.collection.immutable
import scala.collection.mutable.ListBuffer

object EncoderReachability {

    class StateReachability(private val instance: Instance, override val number: Int) extends State(instance, number) {

        override val character: CoordinateVariable = CoordinateVariable(Variable("S" + number + "_C_X", Type.Integer), Variable("S" + number + "_C_Y", Type.Integer))
        override val boxes: immutable.Seq[CoordinateVariable] = instance.boxes.indices
            .map(i => CoordinateVariable(Variable("S" + number + "_B_" + i + "_X", Type.Integer), Variable("S" + number + "_B_" + i + "Y", Type.Integer))).toVector

        val reachabilityNodes: SortedMap[Coordinate, Variable] = SortedMap.empty[Coordinate, Variable]
        val reachabilityWeights: SortedMap[Coordinate, Variable] = SortedMap.empty[Coordinate, Variable]
        val reachabilityEdges: SortedMap[(Coordinate, Coordinate), Variable] = SortedMap.empty[(Coordinate, Coordinate), Variable]

        for ((c, l) <- instance.map.filter(f => f._2.isPlayableArea)) {
            reachabilityNodes.put(c, language.Variable("S" + number + "_RN_X" + c.x + "Y" + c.y, Type.Boolean))
            reachabilityWeights.put(c, Variable("S" + number + "RW_X" + c.x + "Y" + c.y, Type.Integer))

            for (shift <- SokobanAction.ACTIONS.map(f => f.shift)) {
                val end = c + shift
                if (instance.map.contains(end)) {
                    reachabilityEdges.put((c, end), Variable("S" + number + "_RA_X" + c.x + "Y" + c.y + "TX" + end.x + "Y" + end.y, Type.Boolean))
                }
            }
        }

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

    case class BoxAction(private val instance: Instance, private val sT: State, private val sTPlus: State, box: Int) extends Action(sT, sTPlus) {

        override protected val postName: String = "B" + box.toString

        override protected def encode(): ActionEncoding = {

            val terms = ListBuffer.empty[Term]

            val repetitionX = Variable(postName + "_PX", Type.Integer)
            val repetitionY = Variable(postName + "_PY", Type.Integer)

            terms.append(ClauseDeclaration((repetitionX == Integer(0)) || repetitionY == Integer(0)))

            val ands = ListBuffer.empty[Term]

            for (b <- sT.boxes.patch(box, Nil, 1)) { // TODO MEC, Negatius
                ands.append((b.x < sT.boxes(box).x) || (b.x > (sT.boxes(box).x + repetitionX)))
                ands.append((b.y < sT.boxes(box).y) || (b.y > (sT.boxes(box).y + repetitionY)))
            }
            for ((c, _) <- instance.map.filter(f => !f._2.isPlayableArea)) { // TODO MEC, MAL
                ands.append((Integer(c.x) < sT.boxes(box).x) || (Integer(c.x) > (sT.boxes(box).x + repetitionX)))
                ands.append((Integer(c.y) < sT.boxes(box).y) || (Integer(c.y) > (sT.boxes(box).y + repetitionY)))
            }

            val pre = And(ands: _*)
            val eff = (sTPlus.boxes(box).x == sT.boxes(box).x) && (sTPlus.boxes(box).y == sT.boxes(box).y)

            ActionEncoding(pre, eff, terms.toList)
        }
    }
}

class EncoderReachability(override val instance: Instance) extends EncoderBase[StateReachability, Action](instance) {

    def createTimeSteps(nTimeSteps: Int): immutable.Seq[TimeStep[StateReachability, Action]] = {
        val states = (0 to nTimeSteps).map(f => new StateReachability(instance, f)).toList
        states.zip(states).drop(1).map(s =>TimeStep(s._1, s._2, instance.boxes.indices.map(b => BoxAction(instance, s._1, s._2, b))))
    }

    override def encodeInitialState(state: StateReachability): immutable.Seq[Term] = {
        reachability(state)
    }

    override def encodeTimeStep(timeStep: TimeStep[StateReachability, Action]): immutable.Seq[Term] = {
        reachability(timeStep.sTPlus)
    }

    private def reachability(state: StateReachability): immutable.Seq[Term] = {
        val terms = ListBuffer.empty[Term]

        for ((c, l) <- instance.map.iterator.filter(f => f._2.isPlayableArea)) {
            val or = Or((for (b <- instance.boxes.indices) yield {
                (state.boxes(b).x == Integer(c.x)) && (state.boxes(b).y == Integer(c.y))
            }): _*)

            terms.append(ClauseDeclaration(or -> !state.reachabilityNodes.get(c).get))
        }

        for ((c, l) <- instance.map.filter(f => f._2.isPlayableArea)) {
            val nodeStart = state.reachabilityNodes.get(c).get

            val ors = ListBuffer.empty[Term]


            for (end <- SokobanAction.ACTIONS.map(f => c + f.shift).filter(f => existsAndPlayableArea(f))) {
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

    override def decode(assignments: Seq[Assignment], encodingData: EncodingData[StateReachability, Action]): SokobanPlan = {
        val assignmentsMap = assignments.map(f => (f.name, f.value)).toMap

        val actions = ListBuffer.empty[SokobanActionEnum]
        val actionsBalls = ListBuffer.empty[BoxAction]

        val actionsPerformed = encodingData.timeSteps.map(t => (t, t.actions.find(a => assignmentsMap(a.variable.name).asInstanceOf[Boolean]).get))

        for ((timeStep, action)<- actionsPerformed) {
            val sTCharacter = coordinateFromCoordinateVariable(timeStep.sT.character, assignmentsMap)
            val sTPlusCharacter = coordinateFromCoordinateVariable(timeStep.sTPlus.character, assignmentsMap)

            if (sTCharacter.manhattanDistance(sTPlusCharacter) > 1) {
                val allNodes = () => instance.map.filter(f => f._2.isPlayableArea).keys

                val neighbours = (c: Coordinate) => SokobanAction.ACTIONS.map(a => c + a.shift).filter(c => existsAndPlayableArea(c))
                val heuristic = (start: Coordinate, goal: Coordinate) => start.euclideanDistance(goal).toFloat

                val path = AStar.aStar(sTPlusCharacter, sTCharacter, allNodes, neighbours, heuristic) match {
                    case Some(p) =>
                        p
                    case None =>
                        throw PlanInvalidException()
                }

                actions.appendAll(pathToSokobanActions(path))
            } else {
                // TODO
            }

            actions.append(boxActionToSokobanActions(action))
        }

        SokobanPlan(actions.toList)
    }

    private def pathToSokobanActions(path: immutable.Seq[Coordinate]): immutable.Seq[[SokobanActionEnum] = {

    }

    private def boxActionToSokobanActions(boxAction: BoxAction): immutable.Seq[SokobanActionEnum] = {

    }

    private def coordinateFromCoordinateVariable(coordinateVariables: CoordinateVariable, assignmentsMap: immutable.Map[String, Value]): Coordinate = {
        Coordinate(assignmentsMap(coordinateVariables.x.name).asInstanceOf[ValueInteger].v, assignmentsMap(coordinateVariables.y.name).asInstanceOf[ValueInteger].v)
    }


    private def existsAndPlayableArea(coordinate: Coordinate): Boolean = {
        instance.map.get(coordinate) match {
            case Some(o) =>
                o.isPlayableArea
            case None =>
                false
        }
    }
}
