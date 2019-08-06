package org.bitcoins.server

import java.nio.file.Files

import akka.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.db.AppLoggers
import org.bitcoins.node.{SpvNode, SpvNodeCallbacks}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.DataMessageHandler
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.wallet.{LockedWallet, Wallet, WalletStorage}
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object Main extends App {
  implicit val conf = {
    // val custom = ConfigFactory.parseString("bitcoin-s.network = testnet3")
    BitcoinSAppConfig.fromDefaultDatadir()
  }

  private val logger = AppLoggers.getHttpLogger(
    conf.walletConf // doesn't matter which one we pass in
  )

  implicit val walletConf: WalletAppConfig = conf.walletConf
  implicit val nodeConf: NodeAppConfig = conf.nodeConf
  implicit val chainConf: ChainAppConfig = conf.chainConf

  implicit val system = ActorSystem("bitcoin-s")
  import system.dispatcher

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
    Files.exists(walletDB) && WalletStorage.seedExists()
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

  val startFut = for {
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
      SpvNode(peer, bloom, callbacks).start()
    }
    _ = logger.info(s"Starting SPV node sync")
    _ <- node.sync()
    chainApi <- node.chainApiFromDb()
    start <- {
      val walletRoutes = WalletRoutes(wallet, node)
      val nodeRoutes = NodeRoutes(node)
      val chainRoutes = ChainRoutes(chainApi)
      val server = Server(nodeConf, Seq(walletRoutes, nodeRoutes, chainRoutes))

      server.start()
    }
  } yield {

    sys.addShutdownHook {
      logger.error(s"Exiting process")

      node.stop().foreach(_ => logger.info(s"Stopped SPV node"))
      system.terminate().foreach(_ => logger.info(s"Actor system terminated"))
    }

    start
  }

  startFut.failed.foreach { err =>
    logger.info(s"Error on server startup!", err)
  }
}
