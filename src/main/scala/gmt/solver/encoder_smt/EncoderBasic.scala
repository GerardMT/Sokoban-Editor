package gmt.solver.encoder_smt

import gmt.game.SokobanAction
import gmt.game.SokobanAction.SokobanActionEnum
import gmt.instance.{Coordinate, InstanceSokoban}
import gmt.planner.language._
import gmt.planner.language.Integer.ImplicitConstructor
import gmt.planner.planner.ClassicPlanner.{Action, TimeStep}
import gmt.planner.solver.value.Value
import gmt.solver.encoder_smt.EncoderBasic._
import gmt.solver.encoder_smt.EncoderSMT.{ActionSMT, InstanceSMT, StateSMT}

import scala.collection.immutable
import scala.collection.mutable.ListBuffer

object EncoderBasic {

    abstract class CharacterAction(override val instanceSMT: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT) extends ActionSMT(instanceSMT, sT, sTPlus) with RepetitionInterface {

        val repetitionModule = new Repetition(name)

        override val repetition: Variable = repetitionModule.repetition

        protected val direction: Coordinate

        protected def terms(): immutable.Seq[Term] = {
            List(ClauseDeclaration(repetition > Integer(0)))
        }

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

        override def postName: String = "C_U"

        override protected val direction: Coordinate = SokobanAction.UP.shift

        override protected def preconditions(): immutable.Seq[Term] = {
            val pres = ListBuffer.empty[Term]

            for (b <- sT.boxes) {
                pres.append(Or(b.x != sT.character.x, b.y > sT.character.y, b.y < sT.character.y - repetition))
            }

            for (c <- instanceSMT.bounds.walls) {
                pres.append(Or(Integer(c.x) != sT.character.x, Integer(c.y) > sT.character.y, Integer(c.y) < sT.character.y - repetition))
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

    case class DownCharacterAction(override val instanceSMT: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT) extends CharacterAction(instanceSMT, sT, sTPlus) {

        override def postName: String = "C_D"

        override protected val direction: Coordinate = SokobanAction.DOWN.shift

        override protected def preconditions(): immutable.Seq[Term] = {
            val pres = ListBuffer.empty[Term]

            for (b <- sT.boxes) {
                pres.append(Or(b.x != sT.character.x, b.y < sT.character.y, b.y > sT.character.y + repetition))
            }

            for (c <- instanceSMT.bounds.walls) {
                pres.append(Or(Integer(c.x) != sT.character.x, Integer(c.y) < sT.character.y, Integer(c.y) > sT.character.y + repetition))
            }

            pres.toList
        }

        override protected def effects(): immutable.Seq[Term] = {
            val effs = ListBuffer.empty[Term]

            effs.appendAll(super.effects())

            effs.append(sTPlus.character.x == sT.character.x)
            effs.append(sTPlus.character.y == sT.character.y + repetition)

            effs.toList
        }
    }

    case class RightCharacterAction(override val instanceSMT: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT) extends CharacterAction(instanceSMT, sT, sTPlus) {

        override def postName: String = "C_R"

        override protected val direction: Coordinate = SokobanAction.RIGHT.shift

        override protected def preconditions(): immutable.Seq[Term] = {
            val pres = ListBuffer.empty[Term]

            for (b <- sT.boxes) {
                pres.append(Or(b.y != sT.character.y, b.x < sT.character.x, b.x > sT.character.x + repetition))
            }

            for (c <- instanceSMT.bounds.walls) {
                pres.append(Or(Integer(c.y) != sT.character.y, Integer(c.x) < sT.character.x, Integer(c.x) > sT.character.x + repetition))
            }

            pres.toList
        }

        override protected def effects(): immutable.Seq[Term] = {
            val effs = ListBuffer.empty[Term]

            effs.appendAll(super.effects())

            effs.append(sTPlus.character.x == sT.character.x + repetition)
            effs.append(sTPlus.character.y == sT.character.y)

            effs.toList
        }
    }

    case class LeftCharacterAction(override val instanceSMT: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT) extends CharacterAction(instanceSMT, sT, sTPlus) {

        override def postName: String = "C_L"

        override protected val direction: Coordinate = SokobanAction.LEFT.shift

        override protected def preconditions(): immutable.Seq[Term] = {
            val pres = ListBuffer.empty[Term]

            for (b <- sT.boxes) {
                pres.append(Or(b.y != sT.character.y, b.x > sT.character.x, b.x < sT.character.x - repetition))
            }

            for (c <- instanceSMT.bounds.walls) {
                pres.append(Or(Integer(c.y) != sT.character.y, Integer(c.x) > sT.character.x, Integer(c.x) < sT.character.x - repetition))
            }

            pres.toList
        }

        override protected def effects(): immutable.Seq[Term] = {
            val effs = ListBuffer.empty[Term]

            effs.appendAll(super.effects())

            effs.append(sTPlus.character.x == sT.character.x - repetition)
            effs.append(sTPlus.character.y == sT.character.y)

            effs.toList
        }
    }

    case class BoxActionBasic(direction: Coordinate, override val instanceSMT: InstanceSMT, override val sT: StateSMT, override val sTPlus: StateSMT, box: Int) extends ActionSMT(instanceSMT, sT, sTPlus) with RepetitionInterface {

        val repetitionModule = new Repetition(name)

        override val repetition: Variable = repetitionModule.repetition

        override def postName: String = "B" + box.toString + directionName

        private def directionName: String = direction match {
            case SokobanAction.UP.shift =>
                "U"
            case SokobanAction.DOWN.shift =>
                "D"
            case SokobanAction.LEFT.shift =>
                "L"
            case SokobanAction.RIGHT.shift =>
                "R"
        }

        protected def terms(): immutable.Seq[Term] = {
            List(ClauseDeclaration(repetition > Integer(0)))
        }

        protected def preconditions(): immutable.Seq[Term] = {
            preconditionCharacter() ++ preconditionBoxesBetween() ++ preconditionWallsBetween()
        }

        protected def preconditionCharacter(): immutable.Seq[Term] = direction match {
            case SokobanAction.UP.shift =>
                List(sT.boxes(box).x == sT.character.x,
                    sT.boxes(box).y == sT.character.y - Integer(1))

            case SokobanAction.DOWN.shift =>
                List(sT.boxes(box).x == sT.character.x,
                    sT.boxes(box).y == sT.character.y + Integer(1))

            case SokobanAction.RIGHT.shift =>
                List(sT.boxes(box).x == sT.character.x + Integer(1),
                    sT.boxes(box).y == sT.character.y)

            case SokobanAction.LEFT.shift =>
                List(sT.boxes(box).x == sT.character.x - Integer(1),
                    sT.boxes(box).y == sT.character.y)
        }

        protected def preconditionBoxesBetween(): immutable.Seq[Term] = direction match {
            case SokobanAction.UP.shift =>
                for (b <- sT.boxes.patch(box, Nil, 1)) yield {
                    Or(b.x != sT.boxes(box).x, b.y > sT.boxes(box).y + 1, b.y < sT.boxes(box).y - repetition)
                }

            case SokobanAction.DOWN.shift =>
                for (b <- sT.boxes.patch(box, Nil, 1)) yield {
                    Or(b.x != sT.boxes(box).x, b.y < sT.boxes(box).y - 1, b.y > sT.boxes(box).y + repetition)
                }

            case SokobanAction.RIGHT.shift =>
                for (b <- sT.boxes.patch(box, Nil, 1)) yield {
                    Or(b.y != sT.boxes(box).y, b.x < sT.boxes(box).x - 1, b.x > sT.boxes(box).x + repetition)
                }

            case SokobanAction.LEFT.shift =>
                for (b <- sT.boxes.patch(box, Nil, 1)) yield {
                    Or(b.y != sT.boxes(box).y, b.x > sT.boxes(box).x + 1, b.x < sT.boxes(box).x - repetition)
                }
        }

        protected def preconditionWallsBetween(): immutable.Seq[Term] = direction match {
            case SokobanAction.UP.shift =>
                for (c <- instanceSMT.bounds.walls) yield {
                    Or(Integer(c.x) != sT.boxes(box).x, Integer(c.y) > sT.boxes(box).y + 1, Integer(c.y) < sT.boxes(box).y - repetition)
                }

            case SokobanAction.DOWN.shift =>
                for (c <- instanceSMT.bounds.walls) yield {
                    Or(Integer(c.x) != sT.boxes(box).x, Integer(c.y) < sT.boxes(box).y - 1, Integer(c.y) > sT.boxes(box).y + repetition)
                }

            case SokobanAction.RIGHT.shift =>
                for (c <- instanceSMT.bounds.walls) yield {
                    Or(Integer(c.y) != sT.boxes(box).y, Integer(c.x) < sT.boxes(box).x - 1, Integer(c.x) > sT.boxes(box).x + repetition)
                }

            case SokobanAction.LEFT.shift =>
                for (c <- instanceSMT.bounds.walls) yield {
                    Or(Integer(c.y) != sT.boxes(box).y, Integer(c.x) > sT.boxes(box).x + 1, Integer(c.x) < sT.boxes(box).x - repetition)
                }
        }

        protected def effects(): immutable.Seq[Term] = {
            val effs = ListBuffer.empty[Term]

            for (b <- instanceSMT.boxes.indices.patch(box, Nil, 1)) {
                effs.append(sTPlus.boxes(b).x == sT.boxes(b).x)
                effs.append(sTPlus.boxes(b).y == sT.boxes(b).y)
            }

            direction match {
                case SokobanAction.UP.shift =>
                    effs.append(sTPlus.boxes(box).x == sT.boxes(box).x)
                    effs.append(sTPlus.boxes(box).y == sT.boxes(box).y - repetition)

                    effs.append(sTPlus.character.x == sTPlus.boxes(box).x)
                    effs.append(sTPlus.character.y == sTPlus.boxes(box).y + Integer(1))

                case SokobanAction.DOWN.shift =>
                    effs.append(sTPlus.boxes(box).x == sT.boxes(box).x)
                    effs.append(sTPlus.boxes(box).y == sT.boxes(box).y + repetition)

                    effs.append(sTPlus.character.x == sTPlus.boxes(box).x)
                    effs.append(sTPlus.character.y == sTPlus.boxes(box).y - Integer(1))

                case SokobanAction.RIGHT.shift =>
                    effs.append(sTPlus.boxes(box).x == sT.boxes(box).x + repetition)
                    effs.append(sTPlus.boxes(box).y == sT.boxes(box).y)

                    effs.append(sTPlus.character.x == sTPlus.boxes(box).x - Integer(1))
                    effs.append(sTPlus.character.y == sTPlus.boxes(box).y)

                case SokobanAction.LEFT.shift =>
                    effs.append(sTPlus.boxes(box).x == sT.boxes(box).x - repetition)
                    effs.append(sTPlus.boxes(box).y == sT.boxes(box).y)

                    effs.append(sTPlus.character.x == sTPlus.boxes(box).x + Integer(1))
                    effs.append(sTPlus.character.y == sTPlus.boxes(box).y)
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
}

class EncoderBasic(override val instance: InstanceSokoban) extends EncoderSMT[StateSMT](instance) {

    override def createState(number: Int): StateSMT = new StateSMT(number, instance)

    override def createActions(sT: StateSMT, sTPlus: StateSMT): immutable.Seq[Action[StateSMT, SokobanActionEnum]] = {
        List(UpCharacterAction(instanceSMT, sT, sTPlus), DownCharacterAction(instanceSMT, sT, sTPlus), RightCharacterAction(instanceSMT, sT, sTPlus), LeftCharacterAction(instanceSMT, sT, sTPlus)) ++
            instance.boxes.indices.flatMap(b => SokobanAction.VALUES.map(f => BoxActionBasic(f.shift, instanceSMT, sT, sTPlus, b)))
    }

    override val name: String = "EncoderBasic"
}
