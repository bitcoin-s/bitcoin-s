package org.bitcoins.chain.blockchain

import com.typesafe.config.ConfigFactory
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.chain.BlockHeaderHelper
import org.scalameter
import org.scalameter.Bench

object BlockchainBench extends Bench.OfflineReport {
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

  performance of "Blockchain" in {
    measure method "connectTip" in {
      using(headerToConnect)
        .config(
          exec.jvmflags -> List("-Xmx128m")
        )
        .in {
          case (header: BlockHeader, blockchain: Blockchain) =>
            Blockchain.connectTip(header, Vector(blockchain))
        }
    }
  }

  override def persistor: Persistor =
    new SerializationPersistor("target/results")
}
