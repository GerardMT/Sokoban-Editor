package gmt.solver

import gmt.instance.Instance
import gmt.planner.planner.{Planner, PlannerOptions}
import gmt.planner.solver.Yices2Solver
import gmt.planner.translator.SMTLib2
import gmt.solver.encoder_smt.EncoderReachability

class SokobanSolver(val yicesPath: String) {

    def solveSMTEncoding(instance: Instance): Unit = {
        val encoder = new EncoderReachability(instance)
        val translator = new SMTLib2(SMTLib2.QF_LIA)
        val solver = new Yices2Solver(yicesPath)

        new Planner(PlannerOptions(None, None)).solve(encoder, translator, solver)
    }
}
