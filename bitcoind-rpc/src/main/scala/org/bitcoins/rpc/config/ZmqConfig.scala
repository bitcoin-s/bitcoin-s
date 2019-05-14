package org.bitcoins.rpc.config

import java.net.URI

sealed trait ZmqConfig {
  def hashBlock: Option[URI]
  def rawBlock: Option[URI]
  def hashTx: Option[URI]
  def rawTx: Option[URI]
}

object ZmqConfig {
  private case class ZmqConfigImpl(
      hashBlock: Option[URI],
      rawBlock: Option[URI],
      hashTx: Option[URI],
      rawTx: Option[URI]
  ) extends ZmqConfig

  def apply(
      hashBlock: Option[URI] = None,
      rawBlock: Option[URI] = None,
      hashTx: Option[URI] = None,
      rawTx: Option[URI] = None
  ): ZmqConfig =
    ZmqConfigImpl(hashBlock = hashBlock,
                  rawBlock = rawBlock,
                  hashTx = hashTx,
                  rawTx = rawTx)

  /**
    * Creates a `ZmqConfig` with all `URI`s set to
    * `localhost` and the same port
    */
  def fromPort(port: Int): ZmqConfig = {
    val uri = new URI(s"tcp://localhost:$port")
    ZmqConfig(hashBlock = Some(uri),
              rawBlock = Some(uri),
              hashTx = Some(uri),
              rawTx = Some(uri))
  }

  def fromConfig(config: BitcoindConfig): ZmqConfig =
    ZmqConfig(hashBlock = config.zmqpubhashblock,
              hashTx = config.zmqpubhashtx,
              rawBlock = config.zmqpubrawblock,
              rawTx = config.zmqpubrawtx)

}
