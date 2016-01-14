package org.scalacoin.marshallers.transaction

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
  def size : Int
}
