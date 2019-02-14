package org.bitcoins.core.number

import scala.util.Try

trait BasicArithmetic[N] {

  def +(n: N): N

  /**
    * Some classes have restrictions on upper bounds
    * for it's underlying value. This might cause the `+`
    * operator to throw. This method wraps it in a `Try`
    * block.
    */
  def safeAdd(n: N): Try[N] = Try { this + n }

  def -(n: N): N

  /**
    * Some classes have restrictions on lower bounds
    * for it's underlying value. This might cause the `-`
    * operator to throw. This method wraps it in a `Try`
    * block.
    */
  def safeSubtract(n: N): Try[N] = Try { this - n }

  def *(factor: BigInt): N

  /**
    * Some classes have restrictions on upper bounds
    * for it's underlying value. This might cause the `*`
    * operator to throw. This method wraps it in a `Try`
    * block.
    */
  def safeMultiply(factor: BigInt): Try[N] = Try { this * factor }
}
