package gmt.solver

import gmt.game.SokobanAction.SokobanActionEnum

import scala.collection.immutable

case class SokobanPlan(actions: immutable.Seq[SokobanActionEnum])