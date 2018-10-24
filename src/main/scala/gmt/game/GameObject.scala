package gmt.game

object GameObject {

    sealed abstract class GameObjectEnum(val char: Char, val isPlayableArea: Boolean)

    case object EMPTY extends GameObjectEnum('-', false)
    case object CHARACTER extends GameObjectEnum('@', true)
    case object BOX extends GameObjectEnum('$', true)
    case object WALL  extends GameObjectEnum('#', false)
    case object GOAL extends GameObjectEnum('.', true)
    case object GOAL_BOX extends GameObjectEnum('*', true)
}
