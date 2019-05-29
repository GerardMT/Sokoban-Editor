package gmt.game

object GameObject {

    case class InvalidObjectOperationException() extends Exception

    val VALUES = List(ALIGN, EMPTY, CHARACTER, BOX, WALL, GOAL, GOAL_BOX, GOAL_CHARACTER)

    private val characterMap = VALUES.map(f => (f.char, f)).toMap

    sealed abstract class GameObjectEnum(val char: Char, val isPlayableArea: Boolean) {

        def +(that: GameObjectEnum): GameObjectEnum = throw InvalidObjectOperationException()

        def -(that: GameObjectEnum): GameObjectEnum = throw InvalidObjectOperationException()
    }

    case object ALIGN extends GameObjectEnum('x', false)

    case object EMPTY extends GameObjectEnum(' ', true) {

        override def +(that: GameObjectEnum): GameObjectEnum = that match {
            case BOX => BOX
            case CHARACTER => CHARACTER
            case _ => throw InvalidObjectOperationException()
        }
    }

    case object CHARACTER extends GameObjectEnum('@', true) {

        override def +(that: GameObjectEnum): GameObjectEnum = that match {
            case GOAL => GOAL_CHARACTER
            case _ => throw InvalidObjectOperationException()
        }
    }

    case object GOAL_CHARACTER extends GameObjectEnum('+', true) {

        override def -(that: GameObjectEnum): GameObjectEnum = that match {
            case CHARACTER => GOAL
            case GOAL => CHARACTER
            case _ => throw InvalidObjectOperationException()
        }
    }

    case object BOX extends GameObjectEnum('$', true) {

        override def +(that: GameObjectEnum): GameObjectEnum = that match {
            case GOAL => GOAL_BOX
            case _ => throw InvalidObjectOperationException()
        }
    }

    case object WALL  extends GameObjectEnum('#', false)

    case object GOAL extends GameObjectEnum('.', true) {

        override def +(that: GameObjectEnum): GameObjectEnum = that match {
            case BOX => GOAL_BOX
            case CHARACTER => GOAL_CHARACTER
            case _ => throw InvalidObjectOperationException()
        }
    }

    case object GOAL_BOX extends GameObjectEnum('*', true) {

        override def -(that: GameObjectEnum): GameObjectEnum = that match {
            case BOX => GOAL
            case GOAL => BOX
            case _ => throw InvalidObjectOperationException()
        }
    }

    def fromCharacter(char: Char): Option[GameObjectEnum] = characterMap.get(char)
}
