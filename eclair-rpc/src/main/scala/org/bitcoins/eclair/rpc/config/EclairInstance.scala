package org.bitcoins.eclair.rpc.config

import java.io.File
import java.net.URI

import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}
import org.bitcoins.core.protocol.ln.LnPolicy

sealed trait EclairInstance {
  def network: NetworkParameters
  def uri: URI
  def rpcUri: URI
  def authCredentials: EclairAuthCredentials

  def copyWithDatadir(datadir: File): EclairInstance = {
    EclairInstance(
      network = network,
      uri = uri,
      rpcUri = rpcUri,
      authCredentials = authCredentials.copyWithDatadir(datadir))
  }
}

object EclairInstance {
  private case class EclairInstanceImpl(
    network: NetworkParameters,
    uri: URI,
    rpcUri: URI,
    authCredentials: EclairAuthCredentials) extends EclairInstance

  def apply(
    network: NetworkParameters,
    uri: URI,
    rpcUri: URI,
    authCredentials: EclairAuthCredentials): EclairInstance = {
    EclairInstanceImpl(network, uri, rpcUri, authCredentials)
  }

  def fromDatadir(datadir: File): EclairInstance = {
    val eclairConf = new File(datadir.getAbsolutePath + "/eclair.conf")
    val config = ConfigFactory.parseFile(eclairConf)
    val instance = fromConfig(config)
    instance.copyWithDatadir(datadir)

  }
  /**
    * Parses a [[com.typesafe.config.Config Config]] in the format of this
    * [[https://github.com/ACINQ/eclair/blob/master/eclair-core/src/main/resources/reference.conf sample reference.conf]]
    * file to a
    * [[org.bitcoins.eclair.rpc.config.EclairInstance EclairInstance]]
    */
  def fromConfig(config: Config): EclairInstance = {
    val chain = config.getString("eclair.chain")



    val serverBindingIp = config.getString("eclair.server.binding-ip")
    val serverPort = ConfigUtil.getIntOrElse(config, "eclair.server.port", LnPolicy.DEFAULT_LN_P2P_PORT)

    val rpcHost = config.getString("eclair.api.binding-ip")
    val rpcPort = ConfigUtil.getIntOrElse(config, "eclair.api.port", LnPolicy.DEFAULT_ECLAIR_API_PORT)

    val np: NetworkParameters = chain match {
      case "regtest" => RegTest
      case "testnet" => TestNet3
      case "mainnet" => MainNet
      case network: String => throw new IllegalArgumentException(s"Unknown network $network in eclair.conf")
    }

    val uri: URI = new URI(s"http://$serverBindingIp:$serverPort")

    val rpcUri: URI = new URI(s"http://$rpcHost:$rpcPort")

    val eclairAuth = EclairAuthCredentials.fromConfig(config)

    val instance = EclairInstance(
      network = np,
      uri = uri,
      rpcUri = rpcUri,
      authCredentials = eclairAuth)

    instance
  }
}
