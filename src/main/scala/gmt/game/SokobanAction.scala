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

        def boxAction: Boolean
    }

    case object RIGHT extends SokobanActionEnum(Coordinate(+1, +0), "r") {
        override def boxAction: Boolean = false
    }

    case object LEFT extends SokobanActionEnum(Coordinate(-1, +0), "l") {
        override def boxAction: Boolean = false
    }

    case object UP extends SokobanActionEnum(Coordinate(+0, -1), "u") {
        override def boxAction: Boolean = false
    }

    case object DOWN extends SokobanActionEnum(Coordinate(+0, +1), "d") {
        override def boxAction: Boolean = false
    }

    case object RIGHT_BOX extends SokobanActionEnum(Coordinate(+1, +0), "R") {
        override def boxAction: Boolean = true
    }

    case object LEFT_BOX extends SokobanActionEnum(Coordinate(-1, +0), "L") {
        override def boxAction: Boolean = true
    }

    case object UP_BOX extends SokobanActionEnum(Coordinate(+0, -1), "U") {
        override def boxAction: Boolean = true
    }

    case object DOWN_BOX extends SokobanActionEnum(Coordinate(+0, +1), "D") {
        override def boxAction: Boolean = true
    }

    def characterFromShift(shit: Coordinate): Option[SokobanActionEnum] = characterShitMap.get(shit)

    def fromKey(key: String): Option[SokobanActionEnum] = keyMap.get(key)
}
