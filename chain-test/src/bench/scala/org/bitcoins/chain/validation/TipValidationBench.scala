package org.bitcoins.chain.validation

import com.typesafe.config.ConfigFactory
import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.chain.BlockHeaderHelper
import org.scalameter
import org.scalameter.Bench

import scala.concurrent.ExecutionContext

object TipValidationBench
    extends Bench.OfflineReport
    with java.io.Serializable {
  import org.scalameter.api._
  import org.scalameter.picklers.noPickler._

  /**
    * Behaves exactly like the default conf, execpt
    * network is set to mainnet
    */
  lazy val mainnetAppConfig: ChainAppConfig = {
    val mainnetConf = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    BitcoinSTestAppConfig.getTestConfig(mainnetConf)
  }
  // we're working with mainnet data
  implicit lazy val appConfig: ChainAppConfig = mainnetAppConfig

  val newValidTip = BlockHeaderHelper.header1
  val currentTipDb = BlockHeaderHelper.header2Db

  val headerToConnect: scalameter.Gen[(BlockHeader, Blockchain)] = {
    val blockchain = Blockchain.fromHeaders(Vector(currentTipDb))
    scalameter.Gen.single("header")(newValidTip, blockchain)
  }

  performance of "TipValidation" in {
    measure method "checkNewTip" in {
      using(headerToConnect)
        .config(
          )
        .in {
          case (header: BlockHeader, blockchain: Blockchain) =>
            val result = TipValidation.checkNewTip(header, blockchain)
        }
    }
  }

  // GZIPJSONSerializationPersistor is default but we want to choose custom path for regression data
  override def persistor: Persistor =
    new SerializationPersistor("target/results")
}
