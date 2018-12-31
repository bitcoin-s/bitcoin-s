package org.bitcoins.rpc.config
import java.net.URI

import com.typesafe.config.Config

import scala.util.Try

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

  def fromConfig(config: Config): ZmqConfig =
    ZmqConfig(hashBlock = hashBlockUri(config),
              hashTx = hashTxUri(config),
              rawBlock = rawBlockUri(config),
              rawTx = rawTxUri(config))

  private val RAW_BLOCK_KEY = "zmqpubrawblock"
  private val RAW_TX_KEY = "zmqpubrawtx"
  private val HASH_BLOCK_KEY = "zmqpubhashblock"
  private val HASH_TX_KEY = "zmqpubhashtx"

  private val ZMQ_CONFIG_KEYS =
    List(RAW_TX_KEY, RAW_BLOCK_KEY, HASH_TX_KEY, HASH_BLOCK_KEY)

  private def isValidZmqConfigKey(key: String): Boolean =
    ZMQ_CONFIG_KEYS.contains(key)

  private def getZmqUri(config: Config, path: String): Option[URI] = {
    require(
      isValidZmqConfigKey(path),
      s"$path is not a valid ZMQ config key. Valid keys: ${ZMQ_CONFIG_KEYS.mkString(", ")}")

    if (config.hasPath(path)) {
      Try(config.getString(path))
        .map(str => Some(new URI(str)))
        .getOrElse(throw new IllegalArgumentException(
          s"$path in config is not a valid URI"))
    } else {
      None
    }
  }

  private def rawBlockUri(config: Config): Option[URI] =
    getZmqUri(config, RAW_BLOCK_KEY)

  private def rawTxUri(config: Config): Option[URI] =
    getZmqUri(config, RAW_TX_KEY)

  private def hashBlockUri(config: Config): Option[URI] =
    getZmqUri(config, HASH_BLOCK_KEY)

  private def hashTxUri(config: Config): Option[URI] =
    getZmqUri(config, HASH_TX_KEY)

}
