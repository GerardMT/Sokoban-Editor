package gmt.solver.encoder_smt

import gmt.instance.Instance
import gmt.planner.encoder.{Encoder, EncoderResult, Encoding}
import gmt.planner.language._
import gmt.solver.SokobanPlan
import gmt.solver.encoder_smt.EncoderBase._

import scala.collection.immutable
import scala.collection.mutable.ListBuffer

object EncoderBase {

    case class EncodingData[S, A](timeSteps: immutable.Seq[TimeStep[S, A]])

    case class CoordinateVariable(x: Variable, y: Variable)

    trait VariableGenerator {
        def getVariables: immutable.Seq[Variable]
    }

    abstract class State(private val instance: Instance, val number: Int) extends VariableGenerator {
        val character: CoordinateVariable
        val boxes: immutable.Seq[CoordinateVariable]
    }

    abstract class Action(private val sT: State, private val sTPlus: State) extends VariableGenerator {
        val name: String = "S" + sT.number + "_S" + sTPlus.number  + "A_"  + postName

        val variable = Variable(name, Type.Boolean)

        protected val postName: String

        def encode(): ActionEncoding

        def getVariables: immutable.Seq[Variable] = List(variable)
    }

    case class TimeStep[S <: State, A <: Action](sT: S, sTPlus: S, actions: immutable.Seq[A])

    case class ActionEncoding(pre: Term, eff: Term, terms: immutable.Seq[Term])
}

abstract class EncoderBase[S <: State, A <: Action](protected val instance: Instance) extends Encoder[EncodingData[S, A], SokobanPlan] {

    def createTimeSteps(nTimeSteps: Int): immutable.Seq[TimeStep[S, A]]

    def encodeInitialState(state: S): immutable.Seq[Term]

    def encodeTimeStep(timeStep: TimeStep[S, A]): immutable.Seq[Term]

    override def encode(nTimeSteps: Int): EncoderResult[EncodingData[S, A]] = {
        val encoding = new Encoding()

        val timeSteps = createTimeSteps(nTimeSteps)

        for (v <- timeSteps.map(t => t.sT).flatMap(f => f.getVariables)) {
            encoding.add(VariableDeclaration(v))
        }
        encoding.add(timeSteps.last.sTPlus.getVariables: _*)

        for (v <- timeSteps.flatMap(t => t.actions.flatMap(a => a.getVariables))) {
            encoding.add(VariableDeclaration(v))
        }

        encoding.add(encodeInitialState(timeSteps.head.sT): _*)

        for (timeStep <- timeSteps) {
            encoding.add(Operations.eoWithQuatradicAmmo(timeStep.actions.map(f => f.variable)): _*)
            encoding.add(encodeTimeStep(timeStep): _*)

            for (action <- timeStep.actions) {
                val ActionEncoding(pre, eff, terms) = action.encode()

                encoding.add(terms: _*)

                encoding.add(ClauseDeclaration(eff == action.variable))
                encoding.add(ClauseDeclaration(action.variable -> pre))
            }
        }

        encoding.add(goal(timeSteps.last.sTPlus): _*)

        EncoderResult(encoding, EncodingData(timeSteps))
    }

    private def goal(state: S): immutable.Seq[Term] = {
        val terms = ListBuffer.empty[Term]

        for (b <- state.boxes) {
            val or = Or((for (g <- instance.goals) yield {
                ClauseDeclaration((b.x == Integer(g.x)) && (b.y == Integer(g.y)))
            }): _*)
            terms.append(ClauseDeclaration(or))
        }

        terms.toList
    }

    override def lowerBound(): Int = {
        throw UnsupportedOperationException
    }

    override def upperBound(): Int = {
        throw UnsupportedOperationException
    }
}
