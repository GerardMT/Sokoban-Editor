package gmt.game

import gmt.instance.Coordinate

object SokobanAction {

    val VALUES = List(RIGHT, LEFT, UP, DOWN)
    private val shitMap = VALUES.map(f => (f.shift, f)).toMap

    sealed abstract class SokobanActionEnum(val shift: Coordinate, val key: String) {
        override def toString: String = key
    }

    case object RIGHT extends SokobanActionEnum(Coordinate(+1, +0), "right")
    case object LEFT extends SokobanActionEnum(Coordinate(-1, +0), "left")
    case object UP extends SokobanActionEnum(Coordinate(+0, -1), "up")
    case object DOWN extends SokobanActionEnum(Coordinate(+0, +1), "down")

    def fromShift(shit: Coordinate): Option[SokobanActionEnum] = shitMap.get(shit)
}
