package org.bitcoins.core.protocol

import org.bitcoins.core.util.BitcoinSUtil


/**
  * Created by chris on 1/14/16.
  * This represents a element that can be serialized to
  * be sent over the network
  */
trait NetworkElement {

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
  def bytes : Seq[Byte] = BitcoinSUtil.decodeHex(hex)
}
