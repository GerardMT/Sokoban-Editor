package gmt.terminal

import java.io.File

import gmt.instance.InstanceSokoban
import gmt.main.Settings
import gmt.solver.SokobanSolver
import gmt.solver.encoder_smt.{EncoderBasic, EncoderReachability}

import scala.io.Source

object Main {

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

        args.toList match {
            case List("smt_basic", instancePath) =>
                sokobanSolver.solve(new EncoderBasic(loadInstance(instancePath)))
            case List("smt_reachability", instancePath) =>
                sokobanSolver.solve(new EncoderReachability(loadInstance(instancePath)))
            case _ =>
                System.out.println("Unknown arguments")
                sys.exit(0)
        }
    }

    private def loadInstance(instancePath: String): InstanceSokoban = {
        InstanceSokoban.load(Source.fromFile(instancePath).mkString)
    }
}
