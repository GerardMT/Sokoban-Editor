package gmt.instance

import gmt.game.GameObject
import gmt.game.GameObject.{BOX, CHARACTER, GOAL, GameObjectEnum}
import gmt.instance.Instance.{BoxesGoalsDifferentException, NoBoxesException, NoGoalsException}

import scala.collection.mutable.ListBuffer
import scala.collection.{immutable, mutable}

object Instance {

    case class MultipleCharactersException(last: Coordinate, found: Coordinate) extends Exception
    case class NoCharacterException() extends Exception
    case class NoGoalsException() extends Exception
    case class NoBoxesException() extends Exception
    case class BoxesGoalsDifferentException() extends Exception
    case class InvalidCharacterException() extends Exception

    def load(level: String): Instance = {
        val lines = level.tail.split('\n')

        var x = 0
        var y = 0

        val map = mutable.Map.empty[Coordinate, GameObjectEnum]

        var characterOption: Option[Coordinate] = None

        val boxes = ListBuffer.empty[Coordinate]
        val goals= ListBuffer.empty[Coordinate]

        for ((l, y) <- lines.tail.zipWithIndex) {
            for ((c, x) <- l.zipWithIndex) {
                val coordinate = Coordinate(x, y)
                val gameObject = GameObject.getGameObjectEnum(c) match {
                    case Some(o) =>
                        o
                    case None =>
                        throw InvalidCharacterException()
                }

                map(coordinate) = gameObject

                gameObject match {
                    case CHARACTER =>
                        characterOption match {
                            case None =>
                                characterOption = Some(coordinate)
                            case Some(characterCoordinate) =>
                                throw MultipleCharactersException(characterCoordinate, coordinate)
                        }
                    case BOX =>
                        boxes.append(coordinate)
                    case GOAL =>
                        goals.append(coordinate)
                }
            }
        }

        val character = characterOption match {
            case Some(c) =>
                c
            case None =>
                throw NoCharacterException()
        }

        Instance(lines.head, map.toMap, character, boxes.toVector, goals.toVector)
    }
}

case class Instance private (name: String,
                             map: immutable.Map[Coordinate, GameObjectEnum],
                             character: Coordinate,
                             boxes: immutable.Seq[Coordinate],
                             goals: immutable.Seq[Coordinate]) {
    validate()

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
}
