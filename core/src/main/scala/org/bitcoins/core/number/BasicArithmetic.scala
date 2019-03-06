package org.bitcoins.core.number

import scala.util.Try

/**
  * @define mulSafe
  * Some classes have restrictions on upper bounds
  * for it's underlying value. This might cause the `*`
  * operator to throw. This method wraps it in a `Try`
  * block.
  */
trait BasicArithmetic[N] {

  def +(n: N): N

  /**
    * Some classes have restrictions on upper bounds
    * for it's underlying value. This might cause the `+`
    * operator to throw. This method wraps it in a `Try`
    * block.
    */
  def addSafe(n: N): Try[N] = Try { this + n }

  def -(n: N): N

  /**
    * Some classes have restrictions on lower bounds
    * for it's underlying value. This might cause the `-`
    * operator to throw. This method wraps it in a `Try`
    * block.
    */
  def subtractSafe(n: N): Try[N] = Try { this - n }

  def *(factor: BigInt): N

  /**
    * $mulSafe
    */
  def multiplySafe(factor: BigInt): Try[N] = Try { this * factor }

  def *(factor: N): N

  /**
    * $mulSafe
    */
  def multiplySafe(factor: N): Try[N] = Try { this * factor }
}
