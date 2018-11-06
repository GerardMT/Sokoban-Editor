package gmt.game

object GameObject {

    val VALUES = List(ALIGN, EMPTY, CHARACTER, BOX, WALL, GOAL, GOAL_BOX)

    private val characterMap = VALUES.map(f => (f.char, f)).toMap

    sealed abstract class GameObjectEnum(val char: Char, val isPlayableArea: Boolean)

    case object ALIGN extends GameObjectEnum('x', false)
    case object EMPTY extends GameObjectEnum(' ', false)
    case object CHARACTER extends GameObjectEnum('@', true)
    case object BOX extends GameObjectEnum('$', true)
    case object WALL  extends GameObjectEnum('#', false)
    case object GOAL extends GameObjectEnum('.', true)
    case object GOAL_BOX extends GameObjectEnum('*', true)

    def fromCharacter(char: Char): Option[GameObjectEnum] = characterMap.get(char)
}
