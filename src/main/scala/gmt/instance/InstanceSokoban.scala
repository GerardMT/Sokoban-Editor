package gmt.instance

import gmt.game.GameObject
import gmt.game.GameObject._
import gmt.instance.InstanceSokoban.{BoxesGoalsDifferentException, NoBoxesException, NoGoalsException}

import scala.collection.mutable.ListBuffer
import scala.collection.{immutable, mutable}

object InstanceSokoban {

    case class MultipleCharactersException(last: Coordinate, found: Coordinate) extends Exception
    case class NoCharacterException() extends Exception
    case class NoGoalsException() extends Exception
    case class NoBoxesException() extends Exception
    case class BoxesGoalsDifferentException() extends Exception
    case class InvalidCharacterException() extends Exception

    def load(level: String): InstanceSokoban = {

        val lines = level.split('\n')
        val levelLines = lines.tail.map(f => f.toArray)

        for (l <- levelLines) {
            var i = 0
            while (l(i) != WALL.char) {
                l(i) = ALIGN.char
                i += 1
            }
        }

        val map = mutable.Map.empty[Coordinate, GameObjectEnum]

        var characterOption: Option[Coordinate] = None

        val boxes = ListBuffer.empty[Coordinate]
        val goals = ListBuffer.empty[Coordinate]

        var width = 0

        for ((l, y) <- levelLines.zipWithIndex) {
            if (l.length > width) {
                width = l.length
            }

            for ((c, x) <- l.zipWithIndex) {
                val coordinate = Coordinate(x, y)
                val gameObject = GameObject.fromCharacter(c) match {
                    case Some(o) =>
                        o
                    case None =>
                        throw InvalidCharacterException()
                }

                gameObject match {
                    case EMPTY | WALL =>
                        map(coordinate) = gameObject
                    case CHARACTER =>
                        map(coordinate) = gameObject
                        characterOption match {
                            case None =>
                                characterOption = Some(coordinate)
                            case Some(characterCoordinate) =>
                                throw MultipleCharactersException(characterCoordinate, coordinate)
                        }
                    case BOX =>
                        map(coordinate) = gameObject
                        boxes.append(coordinate)
                    case GOAL =>
                        map(coordinate) = gameObject
                        goals.append(coordinate)
                    case _ =>
                        Unit
                }
            }
        }

        val character = characterOption match {
            case Some(c) =>
                c
            case None =>
                throw NoCharacterException()
        }

        InstanceSokoban(lines.head, width, levelLines.length, map.toMap, character, boxes.toVector, goals.toVector)
    }
}

case class InstanceSokoban private(name: String,
                                   width: Int,
                                   height: Int,
                                   map: immutable.Map[Coordinate, GameObjectEnum],
                                   character: Coordinate,
                                   boxes: immutable.Seq[Coordinate],
                                   goals: immutable.Seq[Coordinate]) {
    validate()

    def this(instance: InstanceSokoban) {
        this(instance.name, instance.width, instance.height, instance.map, instance.character, instance.boxes, instance.goals)
    }

    private def validate(): Unit = {
        if (goals.isEmpty) {
            throw NoGoalsException()
        }

        if (boxes.isEmpty) {
            throw NoBoxesException()
        }

        if (boxes.length != goals.length) {
            throw BoxesGoalsDifferentException()
        }
    }

    def existsAndPlayableArea(coordinate: Coordinate): Boolean = {
        map.get(coordinate) match {
            case Some(o) =>
                o.isPlayableArea
            case None =>
                false
        }
    }
}
