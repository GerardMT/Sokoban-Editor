package gmt.solver.encoder_smt

import gmt.game.SokobanAction.SokobanActionEnum
import gmt.instance.InstanceSokoban
import gmt.planner.language._
import gmt.planner.planner.ClassicPlanner.{Action, ActionEncoding, TimeStep}
import gmt.planner.solver.value.Value
import gmt.solver.encoder_smt.EncoderBasic.{BoxActionBasic, CharacterAction}
import gmt.solver.encoder_smt.EncoderSMT.{InstanceSMT, StateSMT}

import scala.collection.immutable
import scala.collection.mutable.ListBuffer

object EncoderBasic {

    case class CharacterAction(instance: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT) extends Action[StateSMT, SokobanActionEnum](sT, sTPlus) with RepetitionInterface {

        override def postName: String = "CA"

        val repetition = new Repetition(name)

        override val repetitionX: Variable = repetition.repetitionX
        override val repetitionY: Variable = repetition.repetitionY

        override def encode(): ActionEncoding = {
            val terms = ListBuffer.empty[Term]

            terms.append(repetition.encode(): _*)

            val pre = And(preconditions(): _*)
            val eff = And(effects(): _*)

            ActionEncoding(pre, eff, terms.toList)
        }

        override def getVariables: immutable.Seq[Variable] = {
            super.getVariables ++ repetition.getVariables
        }

        override def decode(assignments: Map[String, Value]): immutable.Seq[SokobanActionEnum] = {
            repetition.toSokobanActions(assignments)
        }

        protected def preconditions(): immutable.Seq[Term] = { // TODO Inside large bounds and generated walls
            val ands = ListBuffer.empty[Term]

            for (b <- sT.boxes) {
                ands.append(Ite(repetitionX < Integer(0) || repetitionY < Integer(0),
                    ((b.x < sT.character.x) || (b.x > (sT.character.x + repetitionX))) && ((b.y < sT.character.y) || (b.y > (sT.character.y + repetitionY))),
                    ((b.x > sT.character.x) || (b.x < (sT.character.x - repetitionX))) && ((b.y > sT.character.y) || (b.y < (sT.character.y - repetitionY)))))
            }
            for ((c, _) <- instance.map.filter(f => !f._2.isPlayableArea)) {
                ands.append(Ite(repetitionX < Integer(0) || repetitionY < Integer(0),
                    ((Integer(c.x) < sT.character.x) || (Integer(c.x) > (sT.character.x + repetitionX))) && ((Integer(c.y) < sT.character.y) || (Integer(c.y) > (sT.character.y + repetitionY))),
                    ((Integer(c.x) > sT.character.x) || (Integer(c.x) < (sT.character.x - repetitionX))) && ((Integer(c.y) > sT.character.y) || (Integer(c.y) < (sT.character.y - repetitionY)))))
            }

            ands.toList
        }

        protected def effects(): immutable.Seq[Term] = {
            List(sTPlus.character.x == sT.character.x + repetitionX,
            sTPlus.character.y == sT.character.y + repetitionY)
        }
    }

    case class BoxActionBasic(instance: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT, box: Int) extends Action[StateSMT, SokobanActionEnum](sT, sTPlus) with RepetitionInterface {

        override def postName: String = "B" + box.toString

        val repetition = new Repetition(name)

        override val repetitionX: Variable = repetition.repetitionX
        override val repetitionY: Variable = repetition.repetitionY

        override def encode(): ActionEncoding = {
            val terms = ListBuffer.empty[Term]

            terms.append(repetition.encode(): _*)

            val pre = And(preconditions(): _*)
            val eff = And(effects(): _*)

            ActionEncoding(pre, eff, terms.toList)
        }

        protected def preconditions():immutable.Seq[Term] = {  // TODO Inside large bounds and generated walls
            val ands = ListBuffer.empty[Term]

            for (b <- sT.boxes.patch(box, Nil, 1)) {
                ands.append(Ite(repetitionX < Integer(0) || repetitionY < Integer(0),
                    ((b.x < sT.character.x) || (b.x > (sT.character.x + repetitionX + Integer(1)))) && ((b.y < sT.character.y) || (b.y > (sT.character.y + repetitionY + Integer(1)))),
                    ((b.x > sT.character.x) || (b.x < (sT.character.x - repetitionX - Integer(1)))) && ((b.y > sT.character.y) || (b.y < (sT.character.y - repetitionY - Integer(1))))))
            }
            for ((c, _) <- instance.map.filter(f => !f._2.isPlayableArea)) {
                ands.append(Ite(repetitionX < Integer(0) || repetitionY < Integer(0),
                    ((Integer(c.x) < sT.character.x) || (Integer(c.x) > (sT.character.x + repetitionX + Integer(1)))) && ((Integer(c.y) < sT.character.y) || (Integer(c.y) > (sT.character.y + repetitionY + Integer(1)))),
                    ((Integer(c.x) > sT.character.x) || (Integer(c.x) < (sT.character.x - repetitionX - Integer(1)))) && ((Integer(c.y) > sT.character.y) || (Integer(c.y) < (sT.character.y - repetitionY - Integer(1))))))
            }

            ands.append(Ite(repetitionX < Integer(0),
                sT.boxes(box).x == (sT.character.x + Integer(1)),
                sT.boxes(box).y == (sT.character.y + Integer(1))))

            ands.toList
        }

        protected def effects(): immutable.Seq[Term] = {
            List((sTPlus.boxes(box).x == sT.boxes(box).x + repetitionX) && (sTPlus.boxes(box).y == sT.boxes(box).y + repetitionY),
                (sTPlus.character.x == sT.character.x + repetitionX) && (sTPlus.character.y == sT.character.y + repetitionX))
        }

        override def getVariables: immutable.Seq[Variable] = {
            super.getVariables ++ repetition.getVariables
        }

        override def decode(assignments: Map[String, Value]): immutable.Seq[SokobanActionEnum] = {
            repetition.toSokobanActions(assignments)
        }
    }
}

class EncoderBasic(override val instance: InstanceSokoban) extends EncoderSMT[StateSMT](instance) {

    override def createState(number: Int): StateSMT = new StateSMT(number, instance)

    override def createActions(sT: StateSMT, sTPlus: StateSMT): immutable.Seq[Action[StateSMT, SokobanActionEnum]] = {
        CharacterAction(instanceSMT, sT, sTPlus).asInstanceOf[Action[StateSMT, SokobanActionEnum]] +: instance.boxes.indices.map(b => BoxActionBasic(instanceSMT, sT, sTPlus, b).asInstanceOf[Action[StateSMT, SokobanActionEnum]])
    }

    override def encodeInitialState(state: StateSMT): immutable.Seq[Term] = Nil

    override def encodeTimeStep(timeStep: TimeStep[StateSMT, SokobanActionEnum]): immutable.Seq[Term] = Nil
}
