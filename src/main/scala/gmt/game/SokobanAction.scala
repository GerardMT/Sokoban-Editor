package gmt.game

import gmt.instance.Coordinate

object SokobanAction {

    val ACTIONS = List(RIGHT, LEFT, UP, DOWN)

    sealed abstract class SokobanActionEnum(val shift: Coordinate)

    case object RIGHT extends SokobanActionEnum(Coordinate(+1, +0))
    case object LEFT extends SokobanActionEnum(Coordinate(-1, +0))
    case object UP extends SokobanActionEnum(Coordinate(+0, -1))
    case object DOWN extends SokobanActionEnum(Coordinate(+0, +1))
}
