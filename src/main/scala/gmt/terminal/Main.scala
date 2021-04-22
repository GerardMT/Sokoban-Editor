package gmt.terminal

import java.io.{File, PrintWriter}

import gmt.game.GameObject.{ALIGN, BOX, CHARACTER}
import gmt.instance.InstanceSokoban
import gmt.main.Settings
import gmt.planner.fixedPlanner.FixedPlannerResult.FixedPlannerResult
import gmt.planner.planner.ClassicPlanner.ClassicPlannerUpdatesCallback
import gmt.planner.solver.value.Value
import gmt.solver.encoder_smt.EncoderSMT.StateSMT
import gmt.solver.encoder_smt.{EncoderBasic, EncoderReachability, EncoderReachabilityFolding}
import gmt.solver.{SokobanSolver, SolvedSokobanPlan, UnsolvedSokobanPlan}

import scala.collection.mutable
import scala.io.Source

object Main {

    object TerminalPrinter extends ClassicPlannerUpdatesCallback {

        override def plannerUpdate: Option[Function[FixedPlannerResult, _]] = Some(plannerUpdatePrint)

        override def stateDecoded: Option[(StateSMT, Map[String, Value]) => Unit] = Some(stateDecodePrint)

        def plannerUpdatePrint(fixedPlannerResult: FixedPlannerResult): Unit = {
            println("timesteps=" + fixedPlannerResult.timeSteps + " time=" + fixedPlannerResult.milliseconds)
        }

        def stateDecodePrint[A <: StateSMT](state: A, assignments: Map[String, Value]): Unit = {
            val map = mutable.Map(state.instance.emptyMap.toSeq: _*)

            for (b <- state.boxes) {
                val cBox = state.coordinateFromCoordinateVariable(b, assignments)
                map(cBox) = map(cBox) + BOX
            }

            val cCharacter = state.coordinateFromCoordinateVariable(state.character, assignments)
            map(cCharacter) = map(cCharacter) + CHARACTER

            val array = Array.ofDim[Char](state.instance.width, state.instance.height)
            for (x <- 0 until state.instance.width; y <- 0 until state.instance.height) {
                array(x)(y) = ALIGN.char
            }
            for ((c, a) <- map) {
                array(c.x)(c.y) = a.char
            }

            System.out.println("STATE " + state.number)
            for (y <- 0 until state.instance.height) {
                for (x <- 0 until state.instance.width) {
                    System.out.print(array(x)(y))
                }
                System.out.print("\n")
            }
        }
    }

    def updated(fixedPlannerResult: FixedPlannerResult): Unit = {
        println(fixedPlannerResult)
    }

    def getSettings(): Settings = {
        val settingsPath = new File(Main.getClass.getProtectionDomain.getCodeSource.getLocation.toURI).getPath  + "/sokoban_editor.config"
        val file = new File(settingsPath)

        if (!file.exists()) {
            val p = new PrintWriter(file)
            p.write("yices2_path=\n")
            p.close()

            System.out.println("Settings file not found. File created at " + settingsPath + ".")
        }

        val source = Source.fromFile(settingsPath)
        val lines = try source.mkString finally source.close()
        Settings.from(lines)
    }

    def main(args: Array[String]): Unit = {
        args.toList match {
            case List("-h") | List("--help") =>
                System.out.println(
                    """Solve Sokoban levels optimally.
                      |
                      |The editor uses Yices 2 as a SMT solver. The path to this solver is set in the
                      |sokoban_editor.config file located in the same directory as sokoban_editor.jar.
                      |
                      |Usage: java -jar snowman_editor.jar -h | --help
                      |
                      |              Show this message.
                      |
                      |Usage: java -jar sokoban_editor.jar <mode> <level_path>
                      |
                      |<mode>:
                      |
                      |    smt_basic
                      |
                      |         Solve a given level minimizing the character + box movements.
                      |
                      |    smt_reachability
                      |
                      |         Solve a given level minimizing box movements.
                      |
                      |    smt_reachability_folding
                      |
                      |        Solve a given level minimizing change of directions of box movements. A box movement of n
                      |        positions in a straight line has a cost of 1.
                      |
                      |<level_path>:
                      |
                      |        Path to the Sokoban level.
                      |
                      |""".stripMargin)
                
                sys.exit(0)

            case _ =>
        }

        val startTime = System.currentTimeMillis()

        val settings = getSettings()

        val yices2Path = settings.yices2Path match {
            case Some(s) =>
                s
            case None =>
                System.out.println("Yices 2 path must be defined in settings")
                sys.exit(1)
        }

        val sokobanSolver = new SokobanSolver(yices2Path)

        val result = args.toList match {
            case List("smt_basic", instancePath) =>
                sokobanSolver.solve(new EncoderBasic(loadInstance(instancePath), TerminalPrinter), TerminalPrinter)
            case List("smt_reachability", instancePath) =>
                sokobanSolver.solve(new EncoderReachability(loadInstance(instancePath), TerminalPrinter), TerminalPrinter)
            case List("smt_reachability_folding", instancePath) =>
                sokobanSolver.solve(new EncoderReachabilityFolding(loadInstance(instancePath), TerminalPrinter), TerminalPrinter)
            case _ =>
                System.out.println("Unknown arguments")
                sys.exit(1)
        }

        print("\n")

        result match {
            case SolvedSokobanPlan(plan, _) =>
                val planStr = plan.mkString
                println("plan=" + planStr)
                println("lenght=" + planStr.length)
                println("box_actions=" + plan.count(f => f.boxAction))
            case UnsolvedSokobanPlan() =>
                println("Unsolved")
        }

        println("time=" + (System.currentTimeMillis() - startTime) / 1000.0)
    }

    private def loadInstance(instancePath: String): InstanceSokoban = {
        val source = Source.fromFile(instancePath)
        val string = source.mkString
        source.close()
        InstanceSokoban.load(string)
    }
}
