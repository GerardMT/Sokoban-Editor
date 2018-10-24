package gmt.instance

import gmt.game.GameObject.GameObjectEnum

import scala.collection.immutable

object Instance {

    def load(path: String): Instance = {


        Instance()
    }
}

case class Instance private (name: String,
                             map: Map[Coordinate, GameObjectEnum],
                             character: Coordinate,
                             boxes: immutable.Seq[Coordinate],
                             goals: immutable.Seq[Coordinate]) {
    validate()

    private def validate(): Unit = {

    }
}
