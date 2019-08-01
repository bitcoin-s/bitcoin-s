package org.bitcoins.server

import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.node.models.Peer
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import akka.actor.ActorSystem
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
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.wallet.api.UnlockedWalletApi
import org.bitcoins.wallet.api.UnlockWalletSuccess
import org.bitcoins.wallet.api.UnlockWalletError
import org.bitcoins.node.networking.peer.DataMessageHandler
import org.bitcoins.node.SpvNodeCallbacks
import org.bitcoins.wallet.WalletStorage
import org.bitcoins.db.AppLoggers
import org.bitcoins.chain.models.BlockHeaderDAO
import java.util.concurrent.ExecutionException

object ServerMain extends App {
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

  logger.info(s"Starting Bitcoin-S server")
  logger.info(s"Chain: ${nodeConf.chain}")
  logger.info(s"Data directory: ${nodeConf.datadir}")
  logger.info(s"SPV mode enabled: ${nodeConf.isSPVEnabled}")
  logger.info(s"Neutrino mode enabled: ${nodeConf.isNeutrinoEnabled}")
  println(nodeConf.httpLogLevel)

  implicit val system = ActorSystem("bitcoin-s")
  import system.dispatcher

  /** Log the given message, shut down the actor system and quit. */
  def error(message: Any): Nothing = {
    // we might want to log the stack trace if message is an error
    val (processedMessage: Any, stackTrace: Option[Array[StackTraceElement]]) =
      message match {
        case maybeBoxed: ExecutionException =>
          maybeBoxed.getCause() match {
            case null                     => (maybeBoxed, None)
            case err: NotImplementedError => (err, Some(err.getStackTrace()))
            case other: Throwable         => (other, None)
          }
        case notImpl: NotImplementedError =>
          (notImpl, Some(notImpl.getStackTrace()))
        case rest => (rest, None)
      }

    logger.error(s"FATAL: $processedMessage")
    stackTrace.foreach { stack =>
      stack.map(_.toString()).foreach(stackStr => logger.error(s"   $stackStr"))
    }

    // actor system cleanup etc is happening in the shutdown hook
    sys.exit(1)
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
      val blockheaderDAO = BlockHeaderDAO()
      val chain = ChainHandler(blockheaderDAO)
      SpvNode(peer, chain, bloom, callbacks).start()
    }
    _ = logger.info(s"Starting SPV node sync")
    _ <- node.sync()

    start <- {
      val walletRoutes = WalletRoutes(wallet, node)
      val nodeRoutes = NodeRoutes(node)
      val chainRoutes = ChainRoutes(node.chainApi)
      val server =
        Server(nodeConf, // could use either of configurations
               Seq(walletRoutes, nodeRoutes, chainRoutes))
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

  startFut.failed.foreach { error }
}
