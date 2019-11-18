package org.bitcoins.testkit.core.gen

import org.scalacheck.Gen

import scala.annotation.tailrec

object GenUtil {

  @tailrec
  def sample[T](gen: Gen[T]): T = {
    gen.sample match {
      case Some(t) => t
      case None    => sample(gen)
    }
  }
}
