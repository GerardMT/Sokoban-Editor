package gmt.solver.encoder_smt

import gmt.collection.SortedMap
import gmt.game.SokobanAction
import gmt.game.SokobanAction.SokobanActionEnum
import gmt.instance.{Coordinate, Instance}
import gmt.planner.encoder.{Encoder, EncoderResult, Encoding}
import gmt.planner.language
import gmt.planner.language.{And, ClauseDeclaration, Comment, Integer, Operations, Or, Term, Type, Variable, VariableDeclaration}
import gmt.planner.solver.Assignment
import gmt.solver.SokobanPlan
import gmt.solver.encoder_smt.EncoderReachability.{ActionBox, EncodingData, StateReachability}

import scala.collection.immutable
import scala.collection.mutable.ListBuffer

object EncoderReachability {

    case class EncodingData(actions: immutable.Seq[SokobanActionEnum], states: immutable.Seq[StateReachability])

    case class TupleCoordinates(x: Variable, y: Variable)

    trait VariableGenrator {
        def getVariables: immutable.Seq[Variable]
    }

    abstract class State(private val instance: Instance, val number: Int) extends VariableGenrator {
        val character: TupleCoordinates
        val boxes: immutable.Seq[TupleCoordinates]
    }

    class StateReachability(private val instance: Instance, override val number: Int) extends State(instance, number) {

        override val character: TupleCoordinates = TupleCoordinates(Variable("S" + number + "_C_X", Type.Integer), Variable("S" + number + "_C_Y", Type.Integer))
        override val boxes: immutable.Seq[TupleCoordinates] = instance.boxes.indices
            .map(i => TupleCoordinates(Variable("S" + number + "_B_" + i + "_X", Type.Integer), Variable("S" + number + "_B_" + i + "Y", Type.Integer))).toVector

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

    abstract class Action (val sT: StateReachability, val sTPlus: StateReachability) extends VariableGenrator {
        val name: String = "S" + sT.number + "_S" + sTPlus.number  + "A_"  + postName

        protected val postName: String

        val variable = Variable(name, Type.Boolean)

        def getVariables: immutable.Seq[Variable] = List(variable)
    }

    case class ActionBox(override val sT: StateReachability, override val sTPlus: StateReachability, box: Int) extends Action(sT, sTPlus) {

        override protected val postName: String = box.toString
    }
}

class EncoderReachability(private val instance: Instance) extends Encoder[EncodingData, SokobanPlan] {

    override def encode(nTimeSteps: Int): EncoderResult[EncodingData] = {
        val encoding = new Encoding()

        val states = (0 to nTimeSteps).map(f => new StateReachability(instance, f)).toVector
        val timeSteps = states.zip(states).drop(1).map(s => instance.boxes.indices.map(b => ActionBox(s._1, s._2, b)))

        // TODO Add state variables and actions variables to encoding

        encoding.add(reachability(states.head): _*)

        for (s <- states.tail) {
            encoding.add(reachability(s): _*)
        }

        for (timeStep <- timeSteps) {
            val actionVariables = timeStep.map(f => f.variable)
            encoding.add(Operations.eoWithQuatradicAmmo(actionVariables): _*)

            for (a <- timeStep) {
                val ActionEncoderResult(pre, eff, terms) = boxAction(Frame(a.sT, a.sTPlus), a.box, a.name) // TODO To actionBox
                encoding.add(terms: _*)

                encoding.add(ClauseDeclaration(eff == a.variable))
                encoding.add(ClauseDeclaration(a.variable -> pre))
            }
        }

        encoding.add(goal(states.last): _*)

        EncoderResult(encoding, EncodingData(timeSteps))
    }

    private def reachability(state: StateReachability): immutable.Seq[Term] = {
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

        val repetitionX = Variable(name + "_PX", Type.Integer)
        val repetitionY = Variable(name + "_PY", Type.Integer)

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

    private def goal(state: StateReachability): immutable.Seq[Term] = {
        val terms = ListBuffer.empty[Term]

        for (b <- state.boxes) {
            val or = Or((for (g <- instance.goals) yield {
                ClauseDeclaration((b.x == Integer(g.x)) && (b.y == Integer(g.y)))
            }): _*)
            terms.append(ClauseDeclaration(or))
        }

        terms.toList
    }

    override def decode(assignments: Seq[Assignment], encodingData: EncodingData): SokobanPlan = {

    }

    private def existsAndPlayableArea(instance: Instance, coordinate: Coordinate): Boolean = {
        instance.map.get(coordinate) match {
            case Some(o) =>
                o.isPlayableArea
            case None =>
                false
        }
    }

    private case class ActionEncoderResult(pre: Term, post: Term, terms: immutable.Seq[Term])

    private case class Frame(sT: StateReachability, sTPlus: StateReachability)

    private case class Area(c: Coordinate, width: Int, height: Int)

    private case class WallAreas(areas: immutable.Seq[Area], walls: immutable.Seq[Coordinate])
}
