package gmt.solver

import gmt.game.SokobanAction.SokobanActionEnum
import gmt.planner.fixedPlanner.FixedPlannerResult.SomeFixedPlannerResult

import scala.collection.immutable

sealed abstract class SokobanPlan()

case class SolvedSokobanPlan(plan: immutable.Seq[SokobanActionEnum], fixedPlannerResult: SomeFixedPlannerResult) extends SokobanPlan
case class UnsolvedSokobanPlan() extends SokobanPlan