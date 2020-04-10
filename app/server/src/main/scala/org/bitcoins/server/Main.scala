package org.bitcoins.server

import java.net.InetSocketAddress
import java.nio.file.{Files, Paths}

import akka.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.Core
import org.bitcoins.core.api.ChainQueryApi
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db.AppConfig
import org.bitcoins.keymanager.KeyManagerInitializeError
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.DataMessageHandler
import org.bitcoins.node.{NeutrinoNode, Node, NodeCallbacks, SpvNode}
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.AccountDAO
import org.bitcoins.wallet.{LockedWallet, Wallet}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Main extends App {
  implicit val conf = {
    val dataDirIndexOpt = args.zipWithIndex
      .find(_._1.toLowerCase == "--datadir")

    val datadirPath = dataDirIndexOpt match {
      case None => AppConfig.DEFAULT_BITCOIN_S_DATADIR
      case Some((_, dataDirIndex)) =>
        val str = args(dataDirIndex + 1)
        Paths.get(str)
    }
    BitcoinSAppConfig(datadirPath)
  }

  private val logger = HttpLoggerImpl(conf.nodeConf).getLogger

  implicit val walletConf: WalletAppConfig = conf.walletConf
  implicit val nodeConf: NodeAppConfig = conf.nodeConf
  require(nodeConf.isNeutrinoEnabled != nodeConf.isSPVEnabled,
          "Either Neutrino or SPV mode should be enabled")
  implicit val chainConf: ChainAppConfig = conf.chainConf

  implicit val system = ActorSystem("bitcoin-s")
  import system.dispatcher

  val peerSocket =
    parseInetSocketAddress(nodeConf.peers.head, nodeConf.network.port)
  val peer = Peer.fromSocket(peerSocket)
  val bip39PasswordOpt = None //todo need to prompt user for this
  val startFut = for {
    _ <- conf.initialize()

    uninitializedNode <- createNode
    chainApi <- uninitializedNode.chainApiFromDb()
    wallet <- createWallet(uninitializedNode, chainApi, bip39PasswordOpt)
    node <- initializeNode(uninitializedNode, wallet)

    _ <- node.start()
    _ = logger.info(s"Starting SPV node sync")
    _ <- node.sync()
    chainApi <- node.chainApiFromDb()
    start <- {
      val walletRoutes = WalletRoutes(wallet, node)
      val nodeRoutes = NodeRoutes(node)
      val chainRoutes = ChainRoutes(chainApi)
      val coreRoutes = CoreRoutes(Core)
      val server =
        Server(nodeConf, Seq(walletRoutes, nodeRoutes, chainRoutes, coreRoutes))

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
    logger.error(s"Error on server startup!", err)
  }

  /** Checks if the user already has a wallet */
  private def hasWallet(): Future[Boolean] = {
    val walletDB = walletConf.dbPath resolve walletConf.dbName
    val hdCoin = walletConf.defaultAccount.coin
    if (Files.exists(walletDB) && walletConf.seedExists()) {
      AccountDAO().read((hdCoin, 0)).map(_.isDefined)
    } else {
      Future.successful(false)
    }
  }

  private def createNode: Future[Node] = {
    if (nodeConf.isSPVEnabled) {
      Future.successful(SpvNode(peer, nodeConf, chainConf, system))
    } else if (nodeConf.isNeutrinoEnabled) {
      Future.successful(NeutrinoNode(peer, nodeConf, chainConf, system))
    } else {
      Future.failed(
        new RuntimeException("Neither Neutrino nor SPV mode is enabled."))
    }
  }

  private def createWallet(
      nodeApi: Node,
      chainQueryApi: ChainQueryApi,
      bip39PasswordOpt: Option[String]): Future[UnlockedWalletApi] = {
    hasWallet().flatMap { walletExists =>
      if (walletExists) {
        logger.info(s"Using pre-existing wallet")
        val locked = LockedWallet(nodeApi, chainQueryApi)

        // TODO change me when we implement proper password handling
        locked.unlock(BIP39KeyManager.badPassphrase, bip39PasswordOpt) match {
          case Right(wallet) =>
            Future.successful(wallet)
          case Left(kmError) =>
            error(kmError)
        }
      } else {
        logger.info(s"Initializing key manager")
        val bip39PasswordOpt = None
        val keyManagerE: Either[KeyManagerInitializeError, BIP39KeyManager] =
          BIP39KeyManager.initialize(kmParams = walletConf.kmParams,
                                     bip39PasswordOpt = bip39PasswordOpt)

        val keyManager = keyManagerE match {
          case Right(keyManager) => keyManager
          case Left(err) =>
            error(err)
        }

        logger.info(s"Creating new wallet")
        val unInitializedWallet = Wallet(keyManager, nodeApi, chainQueryApi)

        Wallet.initialize(wallet = unInitializedWallet,
                          bip39PasswordOpt = bip39PasswordOpt)
      }
    }
  }

  private def createCallbacks(
      wallet: UnlockedWalletApi): Future[NodeCallbacks] = {
    import DataMessageHandler._
    lazy val onTx: OnTxReceived = { tx =>
      wallet.processTransaction(tx, blockHash = None).map(_ => ())
    }
    lazy val onCompactFilter: OnCompactFilterReceived = {
      (blockHash, blockFilter) =>
        wallet.processCompactFilter(blockHash, blockFilter).map(_ => ())
    }
    lazy val onBlock: OnBlockReceived = { block =>
      wallet.processBlock(block).map(_ => ())
    }
    lazy val onHeaders: OnBlockHeadersReceived = { headers =>
      if (headers.isEmpty) {
        FutureUtil.unit
      } else {
        wallet.updateUtxoPendingStates(headers.last).map(_ => ())
      }
    }
    if (nodeConf.isSPVEnabled) {
      Future.successful(
        NodeCallbacks(onTxReceived = Seq(onTx),
                      onBlockHeadersReceived = Seq(onHeaders)))
    } else if (nodeConf.isNeutrinoEnabled) {
      Future.successful(
        NodeCallbacks(onBlockReceived = Seq(onBlock),
                      onCompactFilterReceived = Seq(onCompactFilter),
                      onBlockHeadersReceived = Seq(onHeaders)))
    } else {
      Future.failed(new RuntimeException("Unexpected node type"))
    }
  }

  private def initializeNode(
      node: Node,
      wallet: UnlockedWalletApi): Future[Node] = {
    for {
      nodeWithBloomFilter <- node match {
        case spvNode: SpvNode =>
          for {
            bloom <- wallet.getBloomFilter()
            _ = logger.info(
              s"Got bloom filter with ${bloom.filterSize.toInt} elements")
          } yield spvNode.setBloomFilter(bloom)
        case _: Node => Future.successful(node)
      }
      callbacks <- createCallbacks(wallet)
    } yield {
      nodeWithBloomFilter.addCallbacks(callbacks)
    }
  }

  /** Log the given message, shut down the actor system and quit. */
  private def error(message: Any): Nothing = {
    logger.error(s"FATAL: $message")
    logger.error(s"Shutting down actor system")
    Await.result(system.terminate(), 10.seconds)
    logger.error("Actor system terminated")
    logger.error(s"Exiting")
    sys.error(message.toString())
  }

  private def parseInetSocketAddress(
      address: String,
      defaultPort: Int): InetSocketAddress = {

    def parsePort(port: String): Int = {
      lazy val errorMsg = s"Invalid peer port: $address"
      try {
        val res = port.toInt
        if (res < 0 || res > 0xffff) {
          throw new RuntimeException(errorMsg)
        }
        res
      } catch {
        case _: NumberFormatException =>
          throw new RuntimeException(errorMsg)
      }
    }

    address.split(":") match {
      case Array(host)       => new InetSocketAddress(host, defaultPort)
      case Array(host, port) => new InetSocketAddress(host, parsePort(port))
      case _                 => throw new RuntimeException(s"Invalid peer address: $address")
    }
  }
}
