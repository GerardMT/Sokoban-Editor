package gmt.solver.encoder_smt

import gmt.collection.SortedMap
import gmt.game.SokobanAction
import gmt.game.SokobanAction.SokobanActionEnum
import gmt.instance.{Coordinate, InstanceSokoban}
import gmt.planner.language
import gmt.planner.language._
import gmt.planner.planner.ClassicPlanner._
import gmt.planner.solver.value.Value
import gmt.solver.EncoderSokoban
import gmt.solver.encoder_smt.EncoderReachabilityBoxes.{ActionReachabilityBoxes, Reachability, StateReachabilityBoxes}
import jdk.jshell.spi.ExecutionControl.NotImplementedException

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer


object EncoderReachabilityBoxes {

    class StateReachabilityBoxes(override val number: Int, val instance: InstanceSokoban) extends State(number) {
        val character: SortedMap[Coordinate, Variable] = SortedMap.empty[Coordinate, Variable]
        val boxes: immutable.Seq[SortedMap[Coordinate, Variable]] = instance.boxes.indices.map(_ => SortedMap.empty[Coordinate, Variable])

        for ((c, _) <- instance.map.filter(f => f._2.isPlayableArea)) {
            character.put(c, Variable("S" + number + "_C_X" + c.x + "Y" + c.y, Type.Boolean))

            for (b <- instance.boxes.indices) {
                boxes(b).put(c, Variable("S" + number + "_B" + b + "_X" + c.x + "Y" + c.y, Type.Boolean))
            }
        }

        override def getVariables: immutable.Seq[Variable] = {
            character.values.toList ++ boxes.flatMap(f => f.values)
        }
    }

    class ActionReachabilityBoxes(instance: InstanceSokoban, override val sT: StateReachabilityBoxes, override val sTPlus: StateReachabilityBoxes, val box: Int, val reachability: Reachability) extends Action[StateReachabilityBoxes, SokobanActionEnum](sT, sTPlus) {

        override protected def postName: String = "B" + box.toString

        override protected def preconditions(): immutable.Seq[Term] = {
            val pres = ListBuffer.empty[Term]

            for ((c, v) <- sT.boxes(box)) {
                pres.append(ClauseDeclaration(v ==> reachability.boxReachabilityNodes(c)))
            }

            pres.toList
        }

        override protected def effects(): immutable.Seq[Term] = {
            val effs = ListBuffer.empty[Term]

            for (b <- sTPlus.boxes.indices.patch(box, Nil, 1); (c, _) <- instance.map.filter(f => f._2.isPlayableArea)) {
                effs.append(ClauseDeclaration(sT.boxes(b)(c) == sTPlus.boxes(b).get(c).get))
            }

            effs.toList
        }

        override def decode(assignments: immutable.Map[String, Value], updatesCallback:ClassicPlannerUpdatesCallback): immutable.Seq[SokobanActionEnum] = {
            val actions = ListBuffer.empty[SokobanActionEnum]

            actions.toList
        }
    }

    class Reachability(instance : InstanceSokoban, number: Int) {
        val characterReachabilityNodes: SortedMap[Coordinate, Variable] = SortedMap.empty[Coordinate, Variable]
        val characterReachabilityEdges: SortedMap[(Coordinate, Coordinate), Variable] = SortedMap.empty[(Coordinate, Coordinate), Variable]
        val characterReachabilityWeights: SortedMap[Coordinate, Variable] = SortedMap.empty[Coordinate, Variable]
        val characterReachabilityTime: SortedMap[Coordinate, Variable] = SortedMap.empty[Coordinate, Variable]

        val boxReachabilityNodes: SortedMap[Coordinate, Variable] = SortedMap.empty[Coordinate, Variable]
        val boxReachabilityEdges: SortedMap[(Coordinate, Coordinate), Variable] = SortedMap.empty[(Coordinate, Coordinate), Variable]
        val boxReachabilityWeights: SortedMap[Coordinate, Variable] = SortedMap.empty[Coordinate, Variable]


        for ((c, _) <- instance.map.filter(f => f._2.isPlayableArea)) {
            characterReachabilityNodes.put(c, language.Variable("S" + number + "_CRN_X" + c.x + "Y" + c.y, Type.Boolean))
            characterReachabilityWeights.put(c, Variable("S" + number + "CRW_X" + c.x + "Y" + c.y, Type.Integer))
            characterReachabilityTime.put(c, Variable("S" + number + "CRT_X" + c.x + "Y" + c.y, Type.Integer))

            boxReachabilityNodes.put(c, language.Variable("S" + number + "_BRN_X" + c.x + "Y" + c.y, Type.Boolean))
            boxReachabilityWeights.put(c, Variable("S" + number + "BRW_X" + c.x + "Y" + c.y, Type.Integer))

            for (shift <- SokobanAction.VALUES_CHARACTER.map(f => f.shift)) {
                val end = c + shift
                if (instance.map.contains(end)) {
                    characterReachabilityEdges.put((c, end), Variable("S" + number + "_CRE_X" + c.x + "Y" + c.y + "TX" + end.x + "Y" + end.y, Type.Boolean))

                    boxReachabilityEdges.put((c, end), Variable("S" + number + "_BRE_X" + c.x + "Y" + c.y + "TX" + end.x + "Y" + end.y, Type.Boolean))
                }
            }
        }

        lazy val terms: immutable.Seq[Term] = {
            val terms = ListBuffer.empty[Term]

            terms.appendAll(characterReachabilityNodes.values.map(f => VariableDeclaration(f)))
            terms.appendAll(characterReachabilityEdges.values.map(f => VariableDeclaration(f)))
            terms.appendAll(characterReachabilityWeights.values.map(f => VariableDeclaration(f)))
            terms.appendAll(characterReachabilityTime.values.map(f => VariableDeclaration(f)))

            throw new NotImplementedException("")

//            for ((c, _) <- instance.map.iterator.filter(f => f._2.isPlayableArea)) {
//                val ors = for (b <- state.boxes) yield {
//                    (b.x == Integer(c.x)) && (b.y == Integer(c.y))
//                }
//
//                val or = Operations.multipleTermsApply(ors, Or.FUNCTION_MULTIPLE)
//
//                terms.append(ClauseDeclaration(or ==> !state.reachabilityNodes.get(c).get))
//            }
//
//            for ((c, _) <- instance.map.filter(f => f._2.isPlayableArea)) {
//                val nodeStart = state.reachabilityNodes.get(c).get
//
//                val ors = ListBuffer.empty[Term]
//
//                for (end <- SokobanAction.VALUES_CHARACTER.map(f => c + f.shift).filter(f => instance.existsAndPlayableArea(f))) {
//                    val edgeInverse = state.reachabilityEdges.get((end, c)).get
//
//                    ors.append(edgeInverse)
//
//                    terms.append(ClauseDeclaration(edgeInverse ==> state.reachabilityNodes.get(end).get))
//                    terms.append(ClauseDeclaration(edgeInverse ==> (state.reachabilityWeights.get(end).get < state.reachabilityWeights.get(c).get)))
//                }
//
//                if (ors.nonEmpty) {
//                    terms.append(ClauseDeclaration(((!(state.character.x == Integer(c.x)) || !(state.character.y == Integer(c.y))) && nodeStart) ==> Operations.multipleTermsApply(ors.toList, Or.FUNCTION_MULTIPLE)))
//                }
//            }

            terms.toList
        }
    }
}

class EncoderReachabilityBoxes(override val instance: InstanceSokoban, override val updatesCallback: ClassicPlannerUpdatesCallback) extends EncoderSokoban[StateReachabilityBoxes](instance, updatesCallback) {

    val reachabilities = mutable.Map.empty[(State, State), Reachability]

    override def createState(number: Int): StateReachabilityBoxes = new StateReachabilityBoxes(number, instance)

    override def createActions(sT: StateReachabilityBoxes, sTPlus: StateReachabilityBoxes): immutable.Seq[Action[StateReachabilityBoxes, SokobanActionEnum]] = {
        val reachability = new Reachability(instance, sT.number)
        reachabilities.put((sT, sTPlus), reachability)

        instance.boxes.indices.map(b => new ActionReachabilityBoxes(instance, sT, sTPlus, b, reachability).asInstanceOf[Action[StateReachabilityBoxes, SokobanActionEnum]])
    }

    override def encodeInitialState(state: StateReachabilityBoxes): immutable.Seq[Term] = {
        val terms = ListBuffer.empty[Term]

        for ((c, v) <- state.character) {
            if (c == instance.character) {
                terms.append(ClauseDeclaration(v))
            } else {
                terms.append(ClauseDeclaration(!v))
            }
        }

        for ((cBox, b) <- instance.boxes.zipWithIndex) {
            for ((c, v) <- state.boxes(b)) {
                if (c == cBox) {
                    terms.append(ClauseDeclaration(v))
                } else {
                    terms.append(ClauseDeclaration(!v))
                }
            }
        }

        terms.toList
    }

    override def encodeAllStates(state: StateReachabilityBoxes): immutable.Seq[Term] = {
        Operations.eoWithQuatradicAmo(state.character.values.toList) ++ state.boxes.flatMap(f => Operations.eoWithQuatradicAmo(f.values.toList))
    }

    override def encodeTimeStep(timeStep: TimeStep[StateReachabilityBoxes, SokobanActionEnum]): immutable.Seq[Term] = {
        reachabilities((timeStep.sT, timeStep.sTPlus)).terms
    }

    override def encodeGoal(state: StateReachabilityBoxes): immutable.Seq[Term] = {
        val terms = ListBuffer.empty[Term]

        for (c <- instance.goals) {
            val orTerms = for (b <- state.boxes) yield {
                b(c)
            }
            terms.append(ClauseDeclaration(Or(orTerms: _*)))
        }

        terms.toList
    }

    override val name: String = "EncoderReachability"
}
