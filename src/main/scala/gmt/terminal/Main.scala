package gmt.terminal

import gmt.instance.InstanceSokoban
import gmt.main.Settings
import gmt.planner.fixedPlanner.FixedPlannerResult.FixedPlannerResult
import gmt.planner.planner.Planner.UpdateListener
import gmt.solver.encoder_smt.{EncoderBasic, EncoderReachability}
import gmt.solver.{SokobanSolver, SolvedSokobanPlan, UnsolvedSokobanPlan}

import scala.io.Source

object Main extends UpdateListener {

    def updated(fixedPlannerResult: FixedPlannerResult): Unit = {
        println(fixedPlannerResult)
    }

    def main(args: Array[String]): Unit = {
        val settingsPath = System.getProperty("user.dir") + "/config"
        val settings = Settings.from(Source.fromFile(settingsPath).getLines.mkString)

        val yices2Path = settings.yices2Path match {
            case Some(s) =>
                s
            case None =>
                System.out.println("Yices2 path must be defined in settings")
                sys.exit(1)
        }

        val sokobanSolver = new SokobanSolver(yices2Path)

        val result = args.toList match {
            case List("smt_basic", instancePath) =>
                sokobanSolver.solve(new EncoderBasic(loadInstance(instancePath)), this)
            case List("smt_reachability", instancePath) =>
                sokobanSolver.solve(new EncoderReachability(loadInstance(instancePath)), this)
            case _ =>
                System.out.println("Unknown arguments")
                sys.exit(0)
        }

        print("\n")

        result match {
            case SolvedSokobanPlan(plan, _) =>
                println(plan.map(f => f + "\n").mkString)
            case UnsolvedSokobanPlan() =>
                println("Unsolved")
        }
    }

    private def loadInstance(instancePath: String): InstanceSokoban = {
        InstanceSokoban.load(Source.fromFile(instancePath).mkString)
    }
}
