package gmt.solver.encoder_smt

import gmt.planner.encoder.{Encoder, EncoderResult}
import gmt.planner.solver.Assignment

abstract class EncoderBase[A, B] extends Encoder[A, B] {

    override def encode(timeSteps: Int): EncoderResult[A] = {

    }

    override def lowerBound(): Int = {
        throw UnsupportedOperationException
    }

    override def upperBound(): Int = {
        throw UnsupportedOperationException
    }

}
