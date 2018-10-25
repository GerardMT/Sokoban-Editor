package gmt.game

import scala.collection.mutable

object GameObject {

    private val characterMap = mutable.Map.empty[Char, GameObjectEnum]

    sealed abstract class GameObjectEnum(val char: Char, val isPlayableArea: Boolean) {
        characterMap(char) = this
    }

    case object EMPTY extends GameObjectEnum('-', false)
    case object CHARACTER extends GameObjectEnum('@', true)
    case object BOX extends GameObjectEnum('$', true)
    case object WALL  extends GameObjectEnum('#', false)
    case object GOAL extends GameObjectEnum('.', true)
    case object GOAL_BOX extends GameObjectEnum('*', true)

    def getGameObjectEnum(char: Char): Option[GameObjectEnum] = characterMap.get(char)
}
