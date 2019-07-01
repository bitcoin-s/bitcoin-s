package org.bitcoins.testkit

import org.scalacheck.Gen
import scala.annotation.tailrec

/**
  * Provides extension methods, syntax
  * and other handy implicit values that
  * aid in testing.
  */
object Implicits {

  /** Extension methods for Scalacheck generatos */
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
}
