package gmt.solver

import gmt.game.SokobanAction.SokobanActionEnum
import gmt.planner.encoder.Encoder
import gmt.planner.fixedPlanner.FixedPlannerResult.SomeFixedPlannerResult
import gmt.planner.planner.ClassicPlanner.ClassicPlannerUpdatesCallback
import gmt.planner.planner.{Planner, PlannerOptions}
import gmt.planner.solver.Yices2Solver
import gmt.planner.translator.SMTLib2

class SokobanSolver(val yicesPath: String, upperBound: Int = 1000) {

    def solve(encoder: Encoder[SokobanActionEnum], updatesCallback: ClassicPlannerUpdatesCallback): SokobanPlan = {
        val translator = new SMTLib2(SMTLib2.QF_LIA)
        val solver = new Yices2Solver(yicesPath)

        val planner = new Planner(PlannerOptions(Some(1), Some(upperBound)), encoder, translator, solver)
        planner.updateListener = updatesCallback.plannerUpdate

        val result = planner.solve()

        val solvedEncodings = result.fixedPlannerResults.filter(f => f.isInstanceOf[SomeFixedPlannerResult]).map(f => f.asInstanceOf[SomeFixedPlannerResult])

        solvedEncodings match {
            case Nil =>
                UnsolvedSokobanPlan()

            case _ =>
                val minimumSolvedEncoding = solvedEncodings.min(Ordering.by((f: SomeFixedPlannerResult) => f.timeSteps))
                val plan = encoder.decode(minimumSolvedEncoding.solverResult.assignments.map(f => (f.name, f.value)).toMap)

                SolvedSokobanPlan(plan, minimumSolvedEncoding)
        }
    }
}
