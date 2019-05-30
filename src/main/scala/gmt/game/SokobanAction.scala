package gmt.game

import gmt.instance.Coordinate

object SokobanAction {

    val VALUES = List(RIGHT, LEFT, UP, DOWN, RIGHT_BOX, LEFT_BOX, UP_BOX, DOWN_BOX)

    val VALUES_CHARACTER = List(RIGHT, LEFT, UP, DOWN)
    val VALUES_BOX = List(RIGHT_BOX, LEFT_BOX, UP_BOX, DOWN_BOX)

    private val characterShitMap = VALUES_CHARACTER.map(f => (f.shift, f)).toMap
    private val keyMap = VALUES.map(f => (f.key, f)).toMap

    sealed abstract class SokobanActionEnum(val shift: Coordinate, val key: String) {
        override def toString: String = key
    }

    case object RIGHT extends SokobanActionEnum(Coordinate(+1, +0), "r")
    case object LEFT extends SokobanActionEnum(Coordinate(-1, +0), "l")
    case object UP extends SokobanActionEnum(Coordinate(+0, -1), "u")
    case object DOWN extends SokobanActionEnum(Coordinate(+0, +1), "d")

    case object RIGHT_BOX extends SokobanActionEnum(Coordinate(+1, +0), "R")
    case object LEFT_BOX extends SokobanActionEnum(Coordinate(-1, +0), "L")
    case object UP_BOX extends SokobanActionEnum(Coordinate(+0, -1), "U")
    case object DOWN_BOX extends SokobanActionEnum(Coordinate(+0, +1), "D")

    def characterFromShift(shit: Coordinate): Option[SokobanActionEnum] = characterShitMap.get(shit)

    def fromKey(key: String): Option[SokobanActionEnum] = keyMap.get(key)
}
