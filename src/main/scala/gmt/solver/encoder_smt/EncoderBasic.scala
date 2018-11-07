package gmt.solver.encoder_smt

import gmt.game.SokobanAction.SokobanActionEnum
import gmt.instance.InstanceSokoban
import gmt.planner.language._
import gmt.planner.planner.ClassicPlanner.{Action, ActionEncoding, TimeStep}
import gmt.planner.solver.value.Value
import gmt.solver.encoder_smt.EncoderBasic.{BoxActionBasic, CharacterAction}
import gmt.solver.encoder_smt.EncoderSMT.{ActionSMT, InstanceSMT, StateSMT}

import scala.collection.immutable
import scala.collection.mutable.ListBuffer

object EncoderBasic {

    case class CharacterAction(override val instanceSMT: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT) extends ActionSMT(instanceSMT, sT, sTPlus) with RepetitionInterface {

        override def postName: String = "CA"

        val repetition = new Repetition(name)

        override val repetitionX: Variable = repetition.repetitionX
        override val repetitionY: Variable = repetition.repetitionY

        override protected def terms(): immutable.Seq[Term] = {
            repetition.encode()
        }

        override protected def preconditions(): immutable.Seq[Term] = {
            val ands = ListBuffer.empty[Term]

            for (b <- sT.boxes) {
                ands.append(Ite(repetitionX < Integer(0) || repetitionY < Integer(0),
                    ((b.x < sT.character.x) || (b.x > (sT.character.x + repetitionX))) && ((b.y < sT.character.y) || (b.y > (sT.character.y + repetitionY))),
                    ((b.x > sT.character.x) || (b.x < (sT.character.x - repetitionX))) && ((b.y > sT.character.y) || (b.y < (sT.character.y - repetitionY)))))
            }
            for ((c, _) <- instanceSMT.map.filter(f => !f._2.isPlayableArea)) {
                ands.append(Ite(repetitionX < Integer(0) || repetitionY < Integer(0),
                    ((Integer(c.x) < sT.character.x) || (Integer(c.x) > (sT.character.x + repetitionX))) && ((Integer(c.y) < sT.character.y) || (Integer(c.y) > (sT.character.y + repetitionY))),
                    ((Integer(c.x) > sT.character.x) || (Integer(c.x) < (sT.character.x - repetitionX))) && ((Integer(c.y) > sT.character.y) || (Integer(c.y) < (sT.character.y - repetitionY)))))
            }

            ands.toList
        }

        override protected def effects(): immutable.Seq[Term] = {
            val effects = ListBuffer.empty[Term]

            effects.append(sTPlus.character.x == sT.character.x + repetitionX)
            effects.append(sTPlus.character.y == sT.character.y + repetitionY)

            for (b <- instanceSMT.boxes.indices) {
                effects.append(sT.boxes(b).x == sTPlus.boxes(b).x)
                effects.append(sT.boxes(b).y == sTPlus.boxes(b).y)
            }

            effects.toList
        }

        override def getVariables: immutable.Seq[Variable] = {
            super.getVariables ++ repetition.getVariables
        }

        override def decode(assignments: Map[String, Value]): immutable.Seq[SokobanActionEnum] = {
            repetition.toSokobanActions(assignments)
        }
    }

    case class BoxActionBasic(override val instanceSMT: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT, box: Int) extends ActionSMT(instanceSMT, sT, sTPlus) with RepetitionInterface {

        override def postName: String = "B" + box.toString

        val repetition = new Repetition(name)

        override val repetitionX: Variable = repetition.repetitionX
        override val repetitionY: Variable = repetition.repetitionY

        override protected def terms(): immutable.Seq[Term] = {
            repetition.encode()
        }

        protected def preconditions():immutable.Seq[Term] = {
            val pres = ListBuffer.empty[Term]

            for (b <- sT.boxes.patch(box, Nil, 1)) {
                pres.append(Ite(repetitionX > Integer(0) || repetitionY > Integer(0),
                    (b.x < sT.character.x || b.x > sT.boxes(box).x + repetitionX) && (b.y < sT.character.y || b.y > sT.boxes(box).y + repetitionY),
                    (b.x > sT.character.x || b.x < sT.boxes(box).x - repetitionX) && (b.y > sT.character.y || b.y < sT.boxes(box).y - repetitionY)))
            }

            for (c <- instanceSMT.bounds.walls) { // TODO BUG quan es mou en eix x, y different o (y igual i (x <) (x <)). Per tant totes les pres depenen de la direccio. Fer 2 accions?
                pres.append(Ite(repetitionX > Integer(0) || repetitionY > Integer(0),
                    (Integer(c.x) < sT.character.x || Integer(c.x) > sT.boxes(box).x + repetitionX) && (Integer(c.y) < sT.character.y || Integer(c.y) > sT.boxes(box).y + repetitionY),
                    (Integer(c.x) > sT.character.x || Integer(c.x) < sT.boxes(box).x - repetitionX) && (Integer(c.y) > sT.character.y || Integer(c.y) < sT.boxes(box).y - repetitionY)))
            }

            pres.append(repetitionX > Integer(0) ==> (sT.boxes(box).x == sT.character.x + Integer(1)))
            pres.append(repetitionX < Integer(0) ==> (sT.boxes(box).x == sT.character.x - Integer(1)))
            pres.append(repetitionY > Integer(0) ==> (sT.boxes(box).y == sT.character.y + Integer(1)))
            pres.append(repetitionY < Integer(0) ==> (sT.boxes(box).y == sT.character.y - Integer(1)))

            pres.append(Boolean(true))

            pres.toList
        }

        protected def effects(): immutable.Seq[Term] = {
            val effs = ListBuffer.empty[Term]

            effs.append(sTPlus.boxes(box).x == sT.boxes(box).x + repetitionX)
            effs.append(sTPlus.boxes(box).y == sT.boxes(box).y + repetitionY)

            effs.append(sTPlus.character.x == sT.character.x + repetitionX)
            effs.append(sTPlus.character.y == sT.character.y + repetitionY)

            for (b <- instanceSMT.boxes.indices.patch(box, Nil, 1)) {
                effs.append(sTPlus.boxes(b).x == sT.boxes(b).x)
                effs.append(sTPlus.boxes(b).y == sT.boxes(b).y)
            }

            effs.toList
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

    override def encodeTimeStep(timeStep: TimeStep[StateSMT, SokobanActionEnum]): immutable.Seq[Term] = Nil
}
