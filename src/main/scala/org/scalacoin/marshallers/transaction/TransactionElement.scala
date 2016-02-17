package org.scalacoin.marshallers.transaction

import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 1/14/16.
 * Represents an element of a transction.
 * Examples would be inputs, outputs, scriptSigs, scriptPubKeys etc.
 */
trait TransactionElement {

  /**
   * The size of the TransactionElement in bytes.
   * @return
   */
  def size : Int = bytes.size

  /**
   * The hexadecimal representation of the transaction element
   * @return
   */
  def hex : String

  /**
   * The byte representation of the transaction element
   * @return
   */
  def bytes : Seq[Byte] = ScalacoinUtil.decodeHex(hex)
}
