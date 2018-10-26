package gmt.solver.encoder_smt

import gmt.instance.Instance
import gmt.planner.language.Term
import gmt.planner.solver.Assignment
import gmt.solver.SokobanPlan
import gmt.solver.encoder_smt.EncoderBase.{Action, ActionEncoding, State, TimeStep}
import gmt.solver.encoder_smt.EncoderBasic.CharacterAction
import gmt.solver.encoder_smt.EncoderReachability.{BoxAction, StateReachability}

import scala.collection.immutable

object EncoderBasic {

    case class CharacterAction(private val instance: Instance, private val sT: State, private val sTPlus: State) extends Action(sT, sTPlus) {
        override protected val postName: String = "CA"

        override def encode(): ActionEncoding = {

            val pre =

            ActionEncoding(pre, eff, Nil)
        }
    }

    case class BoxAction() extends Action() {

    }
}

class EncoderBasic(override val instance: Instance) extends EncoderBase[EncoderBase.State, EncoderBase.Action](instance) {

    override def createTimeSteps(nTimeSteps: Int): immutable.Seq[EncoderBase.TimeStep[EncoderBase.State, EncoderBase.Action]] = {
        val states = (0 to nTimeSteps).map(f => new StateReachability(instance, f)).toList
        states.zip(states).drop(1).map(s =>TimeStep(s._1, s._2, CharacterAction(instance, s._1, s._2) +: instance.boxes.indices.map(b => BoxAction(instance, s._1, s._2, b))))
    }

    override def encodeTimeStep(timeStep: EncoderBase.TimeStep[EncoderBase.State, EncoderBase.Action]): immutable.Seq[Term] = Nil

    override def decode(assignments: Seq[Assignment], encodingData: EncoderBase.EncodingData[EncoderBase.State, EncoderBase.Action]): SokobanPlan = {

    }
}
