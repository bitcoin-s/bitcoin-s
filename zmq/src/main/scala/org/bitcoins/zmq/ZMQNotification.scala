package org.bitcoins.zmq

/** Represents the various notifications we can subscribe
  * to from a zmq publisher
  * [[https://github.com/bitcoin/bitcoin/blob/master/doc/zmq.md#usage]]
  */
sealed abstract class ZMQNotification {
  val topic: String
}

case object HashTx extends ZMQNotification {
  override val topic = "hashtx"
}

case object RawTx extends ZMQNotification {
  override val topic = "rawtx"
}

case object HashBlock extends ZMQNotification {
  override val topic = "hashblock"
}

case object RawBlock extends ZMQNotification {
  override val topic = "rawblock"
}

object ZMQNotification {

  def fromString(str: String): Option[ZMQNotification] =
    str match {
      case HashTx.topic    => Some(HashTx)
      case RawTx.topic     => Some(RawTx)
      case HashBlock.topic => Some(HashBlock)
      case RawBlock.topic  => Some(RawBlock)
      case _               => None
    }
}
