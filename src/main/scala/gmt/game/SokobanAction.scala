package gmt.game

import gmt.instance.Coordinate

object SokobanAction {

    val ACTIONS = List(Right, Left, Up, Down)

    sealed abstract class SokobanActionEnum(val shift: Coordinate)

    case object Right extends SokobanActionEnum(Coordinate(+1, +0))
    case object Left extends SokobanActionEnum(Coordinate(-1, +0))
    case object Up extends SokobanActionEnum(Coordinate(+0, -1))
    case object Down extends SokobanActionEnum(Coordinate(+0, +1))
}
