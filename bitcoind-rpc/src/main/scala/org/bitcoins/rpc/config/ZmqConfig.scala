package org.bitcoins.rpc.config
import java.net.InetSocketAddress

import com.typesafe.config.Config
import org.bitcoins.core.util.BitcoinSLogger

import scala.util.Try

sealed trait ZmqConfig {
  def hashBlock: Option[InetSocketAddress]
  def rawBlock: Option[InetSocketAddress]
  def hashTx: Option[InetSocketAddress]
  def rawTx: Option[InetSocketAddress]
}

object ZmqConfig extends BitcoinSLogger {
  private case class ZmqConfigImpl(
      hashBlock: Option[InetSocketAddress],
      rawBlock: Option[InetSocketAddress],
      hashTx: Option[InetSocketAddress],
      rawTx: Option[InetSocketAddress]
  ) extends ZmqConfig

  def apply(
      hashBlock: Option[InetSocketAddress] = None,
      rawBlock: Option[InetSocketAddress] = None,
      hashTx: Option[InetSocketAddress] = None,
      rawTx: Option[InetSocketAddress] = None
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
    val uri = new InetSocketAddress("tcp://127.0.0.1", port)
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

  private def getZmqUri(
      config: Config,
      path: String): Option[InetSocketAddress] = {
    require(
      isValidZmqConfigKey(path),
      s"$path is not a valid ZMQ config key. Valid keys: ${ZMQ_CONFIG_KEYS.mkString(", ")}")

    if (config.hasPath(path)) {
      Try(config.getString(path))
        .map { str =>
          val hostAndPort = str.split(":")
          val host = hostAndPort(0) + ":" + hostAndPort(1)
          val socket =
            new InetSocketAddress(host, hostAndPort(2).toInt)
          Some(socket)
        }
        .getOrElse(throw new IllegalArgumentException(
          s"$path (${config.getString(path)}) in config is not a valid URI"))
    } else {
      None
    }
  }

  private def rawBlockUri(config: Config): Option[InetSocketAddress] =
    getZmqUri(config, RAW_BLOCK_KEY)

  private def rawTxUri(config: Config): Option[InetSocketAddress] =
    getZmqUri(config, RAW_TX_KEY)

  private def hashBlockUri(config: Config): Option[InetSocketAddress] =
    getZmqUri(config, HASH_BLOCK_KEY)

  private def hashTxUri(config: Config): Option[InetSocketAddress] =
    getZmqUri(config, HASH_TX_KEY)

}
