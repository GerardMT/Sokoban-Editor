import java.io.File

import gmt.game.SokobanAction
import gmt.game.SokobanAction.SokobanActionEnum
import gmt.instance.InstanceSokoban
import gmt.planner.fixedPlanner.FixedPlannerResult
import gmt.planner.planner.ClassicPlanner.State
import gmt.planner.planner.Planner.UpdateListener
import gmt.solver._
import gmt.terminal.Main.getSettings

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Main {

    val SAT_DIRECTORIES = List("../levels/toy")
    val UNSAT_DIRECTORIES = List("../levels/unsat")

    case class Test(instance: InstanceSokoban, testFunction: TestFunction)

    object EmptyUpdateListener extends UpdateListener {
        override def updated(fixedPlannerResult: FixedPlannerResult.FixedPlannerResult): Unit = {}
    }

    abstract case class TestFunction(encoderSokoban: EncoderSokoban[_ <: State], sokobanSolver: SokobanSolver) {
        def applyTest(): Boolean

        val name: String
    }

    trait Sat extends TestFunction {

        def applyTest(): Boolean = {
            sokobanSolver.solve(encoderSokoban, EmptyUpdateListener) match {
                case SolvedSokobanPlan(_, _) =>
                    true
                case UnsolvedSokobanPlan() =>
                    false
            }
        }

        val name: String  = "Sat"
    }

    class SatPlan(override val encoderSokoban: EncoderSokoban[_ <: State], override val sokobanSolver: SokobanSolver, plan: Seq[SokobanActionEnum]) extends TestFunction(encoderSokoban, sokobanSolver) {

        def applyTest(): Boolean = {
            sokobanSolver.solve(encoderSokoban, EmptyUpdateListener) match {
                case SolvedSokobanPlan(planResult, _) =>
                    planResult == plan
                case UnsolvedSokobanPlan() =>
                    false
            }
        }

        val name: String  = "SatPlan"
    }

    trait Unsat extends TestFunction {

        def applyTest(): Boolean = {
            sokobanSolver.solve(encoderSokoban, EmptyUpdateListener) match {
                case SolvedSokobanPlan(_, _) =>
                    false
                case UnsolvedSokobanPlan() =>
                    true
            }
        }

        val name: String  = "Unsat"
    }

    def readLines(path: String): String = {
        val source = Source.fromFile(path)
        try source.mkString finally source.close()
    }

    def listLevels(directory: Seq[String]): Seq[File] = directory.flatMap(d => new File(d).listFiles.filter(f => f.isFile && f.getPath.endsWith(".lvl")).toList)

    def loadPlan(plan: String): Seq[SokobanActionEnum] = {
        plan.linesIterator.toList.tail.map(f => SokobanAction.fromKey(f).get)
    }

    def main(args: Array[String]): Unit = {
        val settings = getSettings()

        val yices2Path = settings.yices2Path match {
            case Some(s) =>
                s
            case None =>
                System.out.println("Yices2 path must be defined in settings")
                sys.exit(1)
        }

        val tests = ListBuffer.empty[Test]

        val sokobanSolver = new SokobanSolver(yices2Path)

        for (f <- listLevels(SAT_DIRECTORIES)) {
            val lines = readLines(f.getAbsolutePath)
            val instance = InstanceSokoban.load(lines)

            for (e <- EncoderFactory(instance)) {
                val planFile = new File(f.getAbsolutePath.dropRight(4) + ".plan")
                val test = if (planFile.exists()) {
                    new SatPlan(e, sokobanSolver, loadPlan(readLines(planFile.getAbsolutePath)))
                } else {
                    new TestFunction(e, sokobanSolver) with Sat
                }
                tests.append(Test(instance, test))
            }
        }

        val sokobanSolverUnsat = new SokobanSolver(yices2Path, 10)
        for (f <- listLevels(UNSAT_DIRECTORIES)) {
            val lines = readLines(f.getAbsolutePath)
            val instance = InstanceSokoban.load(lines)
            for (e <- EncoderFactory(instance)) {
                tests.append(Test(instance, new TestFunction(e, sokobanSolverUnsat) with Unsat))
            }
        }

        for (t <- tests) {
            print(t.instance.name + "    " + t.testFunction.name + "    " + t.testFunction.encoderSokoban.name + "    ")

            if (t.testFunction.applyTest()) {
                println(Console.GREEN + "PASS" + Console.RESET)
            } else {
                println(Console.RED + "FAIL" + Console.RESET)
            }
        }
    }
}
