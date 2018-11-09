package gmt.solver.encoder_smt

import gmt.game.SokobanAction
import gmt.game.SokobanAction.SokobanActionEnum
import gmt.instance.{Coordinate, InstanceSokoban}
import gmt.planner.language._
import gmt.planner.planner.ClassicPlanner.{Action, TimeStep}
import gmt.planner.solver.value.Value
import gmt.solver.encoder_smt.EncoderBasic._
import gmt.solver.encoder_smt.EncoderSMT.{ActionSMT, InstanceSMT, StateSMT}

import scala.collection.immutable
import scala.collection.mutable.ListBuffer

object EncoderBasic {

    abstract class CharacterAction(override val instanceSMT: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT) extends ActionSMT(instanceSMT, sT, sTPlus) with RepetitionInterface {

        override def postName: String = "CA"

        val repetitionModule = new Repetition(name)

        override val repetition: Variable = repetitionModule.repetition

        protected val direction: Coordinate

        override protected def terms(): immutable.Seq[Term] = Nil

        protected def effects(): immutable.Seq[Term] = {
            val effects = ListBuffer.empty[Term]

            for (b <- instanceSMT.boxes.indices) {
                effects.append(sTPlus.boxes(b).x == sT.boxes(b).x)
                effects.append(sTPlus.boxes(b).y == sT.boxes(b).y)
            }

            effects.toList
        }

        override def getVariables: immutable.Seq[Variable] = {
            super.getVariables ++ repetitionModule.getVariables
        }

        def decode(assignments: Map[String, Value]): immutable.Seq[SokobanActionEnum] = { // TODO Extract to function
            val r = repetitionModule.decode(assignments)
            (0 until Math.abs(r)).map(_ => SokobanAction.fromShift(direction).get)
        }
    }

    case class UpCharacterAction(override val instanceSMT: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT) extends CharacterAction(instanceSMT, sT, sTPlus) {

        override protected val direction: Coordinate = SokobanAction.UP.shift

        override protected def preconditions(): immutable.Seq[Term] = {
            val pres = ListBuffer.empty[Term]

            for (b <- sT.boxes) {
                pres.append(Or(b.x != sT.character.x, b.y > sT.character.y, b.y < sT.character.y + repetition))
            }

            pres.toList
        }

        override protected def effects(): immutable.Seq[Term] = {
            val effs = ListBuffer.empty[Term]

            effs.appendAll(super.effects())

            effs.append(sTPlus.character.x == sT.character.x)
            effs.append(sTPlus.character.y == sT.character.y - repetition)

            effs.toList
        }
    }

    // TODO 4 Actions

    abstract class BoxActionBasic(override val instanceSMT: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT, val box: Int) extends ActionSMT(instanceSMT, sT, sTPlus) with RepetitionInterface {

        val repetitionModule = new Repetition(name)

        override val repetition: Variable = repetitionModule.repetition

        override protected def terms(): immutable.Seq[Term] = Nil

        protected val direction: Coordinate

        protected def effects(): immutable.Seq[Term] = {
            val effs = ListBuffer.empty[Term]

            for (b <- instanceSMT.boxes.indices.patch(box, Nil, 1)) {
                effs.append(sTPlus.boxes(b).x == sT.boxes(b).x)
                effs.append(sTPlus.boxes(b).y == sT.boxes(b).y)
            }

            effs.toList
        }

        override def getVariables: immutable.Seq[Variable] = {
            super.getVariables ++ repetitionModule.getVariables
        }

        def decode(assignments: Map[String, Value]): immutable.Seq[SokobanActionEnum] = {
            val r = repetitionModule.decode(assignments)
            (0 until Math.abs(r)).map(_ => SokobanAction.fromShift(direction).get)
        }
    }

    case class UpBoxActionBasic(override val instanceSMT: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT, override val box: Int) extends BoxActionBasic(instanceSMT, sT, sTPlus, box){

        override def postName: String = "B" + box.toString + "_U"

        override val direction = SokobanAction.UP.shift

        protected def preconditions():immutable.Seq[Term] = {
            val pres = ListBuffer.empty[Term]

            for (b <- sT.boxes.patch(box, Nil, 1)) {
                pres.append(Or(b.x != sT.character.x, b.y > sT.character.y, b.y < sT.boxes(box).y + repetition))
            }

            for (c <- instanceSMT.bounds.walls) {
                pres.append(Or(Integer(c.x) != sT.character.x, Integer(c.y) > sT.character.y, Integer(c.y) < sT.boxes(box).y + repetition))
            }

            pres.append(sT.boxes(box).y == sT.character.y - Integer(1))

            pres.toList
        }

        override protected def effects(): immutable.Seq[Term] = {
            val effs = ListBuffer.empty[Term]

            effs.appendAll(super.effects())

            effs.append(sTPlus.boxes(box).x == sT.boxes(box).x)
            effs.append(sTPlus.boxes(box).y == sT.boxes(box).y - repetition)

            effs.append(sTPlus.character.x == sT.character.x)
            effs.append(sTPlus.character.y == sT.character.y - repetition)


            effs.toList
        }
    }

    case class DownBoxActionBasic(override val instanceSMT: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT, override val box: Int) extends BoxActionBasic(instanceSMT, sT, sTPlus, box){

        override def postName: String = "B" + box.toString + "_D"

        override val direction = SokobanAction.DOWN.shift

        protected def preconditions():immutable.Seq[Term] = {
            val pres = ListBuffer.empty[Term]

            for (b <- sT.boxes.patch(box, Nil, 1)) {
                pres.append(Or(b.x != sT.character.x, b.y < sT.character.y, b.y > sT.boxes(box).y - repetition))
            }

            for (c <- instanceSMT.bounds.walls) {
                pres.append(Or(Integer(c.x) != sT.character.x, Integer(c.y) < sT.character.y, Integer(c.y) > sT.boxes(box).y - repetition))
            }

            pres.append(sT.boxes(box).y == sT.character.y + Integer(1))

            pres.toList
        }

        override protected def effects(): immutable.Seq[Term] = {
            val effs = ListBuffer.empty[Term]

            effs.appendAll(super.effects())

            effs.append(sTPlus.boxes(box).x == sT.boxes(box).x)
            effs.append(sTPlus.boxes(box).y == sT.boxes(box).y + repetition)

            effs.append(sTPlus.character.x == sT.character.x)
            effs.append(sTPlus.character.y == sT.character.y + repetition)

            effs.toList
        }
    }

    case class RightBoxActionBasic(override val instanceSMT: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT, override val box: Int) extends BoxActionBasic(instanceSMT, sT, sTPlus, box){

        override def postName: String = "B" + box.toString + "_R"

        override val direction = SokobanAction.RIGHT.shift

        protected def preconditions():immutable.Seq[Term] = {
            val pres = ListBuffer.empty[Term]

            for (b <- sT.boxes.patch(box, Nil, 1)) {
                pres.append(Or(b.y != sT.character.y, b.x > sT.character.x, b.x < sT.boxes(box).x + repetition))
            }

            for (c <- instanceSMT.bounds.walls) {
                pres.append(Or(Integer(c.y) != sT.character.y, Integer(c.x) > sT.character.x, Integer(c.x) < sT.boxes(box).x + repetition))
            }

            pres.append(sT.boxes(box).x == sT.character.x + Integer(1))

            pres.toList
        }

        override protected def effects(): immutable.Seq[Term] = {
            val effs = ListBuffer.empty[Term]

            effs.appendAll(super.effects())

            effs.append(sTPlus.boxes(box).x == sT.boxes(box).x - repetition)
            effs.append(sTPlus.boxes(box).y == sT.boxes(box).y)

            effs.append(sTPlus.character.x == sT.character.x - repetition)
            effs.append(sTPlus.character.y == sT.character.y)

            effs.toList
        }
    }

    case class LeftBoxActionBasic(override val instanceSMT: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT, override val box: Int) extends BoxActionBasic(instanceSMT, sT, sTPlus, box){

        override def postName: String = "B" + box.toString + "_L"

        override val direction = SokobanAction.LEFT.shift

        protected def preconditions():immutable.Seq[Term] = {
            val pres = ListBuffer.empty[Term]

            for (b <- sT.boxes.patch(box, Nil, 1)) {
                pres.append(Or(b.y != sT.character.y, b.x < sT.character.x, b.x > sT.boxes(box).x - repetition))
            }

            for (c <- instanceSMT.bounds.walls) {
                pres.append(Or(Integer(c.y) != sT.character.y, Integer(c.x) < sT.character.x, Integer(c.x) > sT.boxes(box).x - repetition))
            }

            pres.append(sT.boxes(box).x == sT.character.x - Integer(1))

            pres.toList
        }

        override protected def effects(): immutable.Seq[Term] = {
            val effs = ListBuffer.empty[Term]

            effs.appendAll(super.effects())

            effs.append(sTPlus.boxes(box).x == sT.boxes(box).x + repetition)
            effs.append(sTPlus.boxes(box).y == sT.boxes(box).y)

            effs.append(sTPlus.character.x == sT.character.x + repetition)
            effs.append(sTPlus.character.y == sT.character.y)

            effs.toList
        }
    }
}

class EncoderBasic(override val instance: InstanceSokoban) extends EncoderSMT[StateSMT](instance) {

    override def createState(number: Int): StateSMT = new StateSMT(number, instance)

    override def createActions(sT: StateSMT, sTPlus: StateSMT): immutable.Seq[Action[StateSMT, SokobanActionEnum]] = {
//        List(UpCharacterAction(instanceSMT, sT, sTPlus), DownCharacterAction(instanceSMT, sT, sTPlus), RightCharacterAction(instanceSMT, sT, sTPlus), LeftCharacterAction(instanceSMT, sT, sTPlus)) ++
        List(UpCharacterAction(instanceSMT, sT, sTPlus)) ++
        instance.boxes.indices.flatMap(b => List(UpBoxActionBasic(instanceSMT, sT, sTPlus, b), DownBoxActionBasic(instanceSMT, sT, sTPlus, b), RightBoxActionBasic(instanceSMT, sT, sTPlus, b), LeftBoxActionBasic(instanceSMT, sT, sTPlus, b)))
    }

    override def encodeTimeStep(timeStep: TimeStep[StateSMT, SokobanActionEnum]): immutable.Seq[Term] = Nil
}
