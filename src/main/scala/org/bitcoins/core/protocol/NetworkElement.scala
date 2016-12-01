package org.bitcoins.core.protocol

import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil}


/**
  * Created by chris on 1/14/16.
  * This represents a element that can be serialized to
  * be sent over the network
  */
trait NetworkElement extends BitcoinSLogger {

  /**
   * The size of the NetworkElement in bytes.
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
