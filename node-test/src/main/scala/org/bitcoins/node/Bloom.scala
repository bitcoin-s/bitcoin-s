package org.bitcoins.node

import java.net.{InetAddress, InetSocketAddress}

import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderTable}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.models.Peer
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import org.bitcoins.wallet.LockedWallet
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.Wallet
import org.bitcoins.core.crypto.ECPublicKey
import scodec.bits._
import org.bitcoins.wallet.api.CreateWalletApi
import org.bitcoins.wallet.api.InitializeWalletSuccess
import org.bitcoins.wallet.api.InitializeWalletError
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.number.UInt32

object Bloom extends App with BitcoinSLogger {
  implicit val system = Constants.actorSystem
  import system.dispatcher

  implicit val nodeConfig = NodeAppConfig()
  implicit val chainAppConfig = ChainAppConfig()
  implicit val walletConf = WalletAppConfig()

  logger.info(s"Chain config: ${chainAppConfig.dbConfig.config}")

  val bhDAO = BlockHeaderDAO(chainAppConfig)
  val chainApi = ChainHandler(bhDAO, chainAppConfig)
  val table = TableQuery[BlockHeaderTable]

  val chainInitF = chainAppConfig.initialize
  Await.result(chainInitF, 3.seconds)

  val instance = BitcoindInstance.fromDatadir()
  val rpc = new BitcoindRpcClient(instance)

  val peer = Peer.fromBitcoind(instance)

  logger.info(s"Starting spv node")

  val startF = for {
    // bech32
    // address bcrt1qvkvchs2w5r65rfhg56gv7mfxqmc6lu55v05e8p
    // pubkey 0398adca219da43bfe040ec1c5b65525c219608b54613cae2fb0ca6fcd4f9d3885

    // legacy
    // address mijojbWyYiR4np8nQQ3gs3tu9Lw5CzQatm
    // pubkey 0261c9b1e3f0671abdf6d129fce75fde4a071c1a45b7f63743522d533586a729f5

    // wallet <- LockedWallet()
    // wallet <- Wallet.initialize().map {
    // case InitializeWalletSuccess(wallet) => wallet
    // case err: InitializeWalletError =>
    // throw err
    // }

    // address <- wallet.getNewAddress()
    // _ = logger.info(s"Address: $address")
    // info <- wallet.getAddressInfo(address)
    // _ = logger.info(s"Info: $info")
    spv <- SpvNode(peer, chainApi).start()
    _ = logger.info(s"Starting SPV node sync")
    _ <- spv.sync()
    _ = logger.info(s"Started syncing SPV node successfully")

  } yield ()

  startF.onComplete {
    case Failure(exception) =>
      logger.error(s"${exception.getMessage}")
    case Success(_) =>
      logger.info(s"startF complete")
  }

}
