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

    @tailrec
    /** Gets a sample from this generator that's not `None` */
    def sampleSome: T = gen.sample match {
      case None         => sampleSome
      case Some(sample) => sample
    }
  }
}
