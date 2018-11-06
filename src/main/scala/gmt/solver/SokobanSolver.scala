package gmt.solver

import gmt.planner.encoder.Encoder
import gmt.planner.planner.{Planner, PlannerOptions}
import gmt.planner.solver.Yices2Solver
import gmt.planner.translator.SMTLib2

class SokobanSolver(val yicesPath: String) {

    def solve(encoder: Encoder[_]): Unit = {
        val translator = new SMTLib2(SMTLib2.QF_LIA)
        val solver = new Yices2Solver(yicesPath)

        new Planner(PlannerOptions(Some(1), Some(1000))).solve(encoder, translator, solver)
    }
}
