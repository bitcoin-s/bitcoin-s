package org.bitcoins.testkitcore

import org.scalacheck.Gen
import org.scalatest.compatible.Assertion
import org.scalatest.exceptions.TestFailedException

import scala.annotation.tailrec

/** Provides extension methods, syntax
  * and other handy implicit values that
  * aid in testing.
  */
object Implicits {

  /** Extension methods for Scalacheck generators */
  implicit class GeneratorOps[T](private val gen: Gen[T]) extends AnyVal {

    /** Gets a sample from this generator that's not `None` */
    def sampleSome: T = {
      val max = 10
      @tailrec
      def loop(counter: Int): T =
        if (counter > max) {
          sys.error(
            s"Could not get a sample from generator after $max attempts")
        } else {
          gen.sample match {
            case None         => loop(counter + 1)
            case Some(sample) => sample
          }
        }

      loop(0)
    }
  }

  /** Extension methods for sequences of assertions */
  implicit class AssertionSeqOps(private val assertions: Seq[Assertion]) {

    /** Flattens a sequence of assertions into only one */
    def toAssertion: Assertion =
      assertions match {
        case Seq() =>
          throw new TestFailedException(
            message = "Cannot turn an empty list into an assertion!",
            failedCodeStackDepth = 0)
        // this should force all collection kinds to
        // evaluate all their members, throwing when
        // evaluating a bad one
        case nonEmpty =>
          nonEmpty.foreach(_ => ())
          nonEmpty.last
      }
  }
}
