package gmt.solver

import gmt.instance.Instance
import gmt.planner.planner.{Planner, PlannerOptions}
import gmt.planner.solver.Yices2Solver
import gmt.planner.translator.SMTLib2

class Sokoban(val yicesPath: String) {

    def solveSMTEncoding(instance: Instance): Unit = {
        val encoder = new SokobanEncoder(instance)
        val translator = new SMTLib2(SMTLib2.QF_LIA)
        val solver = new Yices2Solver(yicesPath)

        new Planner(PlannerOptions(None, None)).solve(encoder, translator, solver)
    }
}
