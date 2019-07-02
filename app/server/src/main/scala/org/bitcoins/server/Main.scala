package org.bitcoins.server

import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.node.models.Peer
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import akka.actor.ActorSystem
import scala.concurrent.Await
import scala.concurrent.duration._
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.node.config.NodeAppConfig
import java.nio.file.Files
import scala.concurrent.Future
import org.bitcoins.wallet.LockedWallet
import org.bitcoins.wallet.Wallet
import org.bitcoins.wallet.api.InitializeWalletSuccess
import org.bitcoins.wallet.api.InitializeWalletError
import org.bitcoins.node.SpvNode
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.chain.config.ChainAppConfig
import akka.http.scaladsl.server.Directives._
import org.bitcoins.wallet.api.UnlockedWalletApi
import org.bitcoins.wallet.api.UnlockWalletSuccess
import org.bitcoins.wallet.api.UnlockWalletError
import org.bitcoins.node.networking.peer.DataMessageHandler
import org.bitcoins.node.SpvNodeCallbacks

object Main
    extends App
    // TODO we want to log to user data directory
    // how do we do this?
    with BitcoinSLogger {
  implicit val conf = {
    // val custom = ConfigFactory.parseString("bitcoin-s.network = testnet3")
    BitcoinSAppConfig()
  }

  implicit val walletConf: WalletAppConfig = conf.walletConf
  implicit val nodeConf: NodeAppConfig = conf.nodeConf
  implicit val chainConf: ChainAppConfig = conf.chainConf

  implicit val system = ActorSystem("bitcoin-s")
  import system.dispatcher

  sys.addShutdownHook {
    logger.error(s"Exiting process")
    system.terminate().foreach(_ => logger.info(s"Actor system terminated"))
  }

  /** Log the given message, shut down the actor system and quit. */
  def error(message: Any): Nothing = {
    logger.error(s"FATAL: $message")
    logger.error(s"Shutting down actor system")
    Await.result(system.terminate(), 10.seconds)
    logger.error("Actor system terminated")
    logger.error(s"Exiting")
    sys.error(message.toString())
  }

  /** Checks if the user already has a wallet */
  def hasWallet(): Boolean = {
    val walletDB = walletConf.dbPath resolve walletConf.dbName
    Files.exists(walletDB)
  }

  val walletInitF: Future[UnlockedWalletApi] = if (hasWallet()) {
    logger.info(s"Using pre-existing wallet")
    val locked = LockedWallet()

    // TODO change me when we implement proper password handling
    locked.unlock(Wallet.badPassphrase) match {
      case UnlockWalletSuccess(wallet) => Future.successful(wallet)
      case err: UnlockWalletError      => error(err)
    }
  } else {
    logger.info(s"Creating new wallet")
    Wallet.initialize().map {
      case InitializeWalletSuccess(wallet) => wallet
      case err: InitializeWalletError      => error(err)
    }
  }

  val bitcoind = BitcoindInstance.fromDatadir()
  val bitcoindCli = new BitcoindRpcClient(bitcoind)
  val peer = Peer.fromBitcoind(bitcoind)

  for {
    _ <- bitcoindCli.isStartedF.map { started =>
      if (!started) error("Local bitcoind is not started!")
    }
    _ <- bitcoindCli.getBlockChainInfo.map { bitcoindInfo =>
      if (bitcoindInfo.chain != nodeConf.network)
        error(
          s"bitcoind and Bitcoin-S node are on different chains! Bitcoind: ${bitcoindInfo.chain}. Bitcoin-S node: ${nodeConf.network}")
    }

    _ <- conf.initialize()
    wallet <- walletInitF

    bloom <- wallet.getBloomFilter()
    _ = logger.info(s"Got bloom filter with ${bloom.filterSize.toInt} elements")

    node <- {

      val callbacks = {
        import DataMessageHandler._
        val onTX: OnTxReceived = { tx =>
          wallet.processTransaction(tx, confirmations = 0)
          ()
        }

        SpvNodeCallbacks(onTxReceived = Seq(onTX))
      }
      val blockheaderDAO = BlockHeaderDAO()
      val chain = ChainHandler(blockheaderDAO, conf)
      SpvNode(peer, chain, bloom, callbacks).start()
    }
    _ = logger.info(s"Starting SPV node sync")
    _ <- node.sync()

    _ <- {
      val walletRoutes = WalletRoutes(wallet)
      val nodeRoutes = NodeRoutes(node)
      val chainRoutes = ChainRoutes(node.chainApi)
      val server = Server(Seq(walletRoutes, nodeRoutes, chainRoutes))
      server.start()
    }
  } yield ()

}
