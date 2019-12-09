package org.bitcoins.server

import java.net.InetSocketAddress
import java.nio.file.Files

import akka.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.NodeApi
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.DataMessageHandler
import org.bitcoins.node.{NeutrinoNode, Node, NodeCallbacks, SpvNode}
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.{LockedWallet, Wallet}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Main extends App {
  implicit val conf = BitcoinSAppConfig.fromDefaultDatadir()

  private val logger = HttpLogger.getLogger(
    conf.nodeConf // doesn't matter which one we pass in
  )

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

  val startFut = for {
    _ <- conf.initialize()

    uninitializedNode <- createNode
    wallet <- createWallet(uninitializedNode)
    node <- initializeNode(uninitializedNode, wallet)

    _ <- node.start()
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
    logger.error(s"Error on server startup!", err)
  }

  /** Checks if the user already has a wallet */
  private def hasWallet(): Boolean = {
    val walletDB = walletConf.dbPath resolve walletConf.dbName
    Files.exists(walletDB) && walletConf.seedExists()
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

  private def createWallet(nodeApi: NodeApi): Future[UnlockedWalletApi] = {
    if (hasWallet()) {
      logger.info(s"Using pre-existing wallet")
      val locked = LockedWallet(nodeApi)

      // TODO change me when we implement proper password handling
      locked.unlock(Wallet.badPassphrase) match {
        case UnlockWalletSuccess(wallet) => Future.successful(wallet)
        case err: UnlockWalletError      => error(err)
      }
    } else {
      logger.info(s"Creating new wallet")
      Wallet.initialize(nodeApi).map {
        case InitializeWalletSuccess(wallet) => wallet
        case err: InitializeWalletError      => error(err)
      }
    }
  }

  private def createCallbacks(
      wallet: UnlockedWalletApi): Future[NodeCallbacks] = {
    import DataMessageHandler._
    lazy val onTx: OnTxReceived = { tx =>
      wallet.processTransaction(tx, confirmations = 0)
      ()
    }
    lazy val onCompactFilter: OnCompactFilterReceived = {
      (blockHash, blockFilter) =>
        wallet.processCompactFilter(blockHash, blockFilter)
    }
    lazy val onBlock: OnBlockReceived = { block =>
      wallet.processBlock(block, 0)
      ()
    }
    if (nodeConf.isSPVEnabled) {
      Future.successful(NodeCallbacks(onTxReceived = Seq(onTx)))
    } else if (nodeConf.isNeutrinoEnabled) {
      Future.successful(
        NodeCallbacks(onBlockReceived = Seq(onBlock),
                      onCompactFilterReceived = Seq(onCompactFilter)))
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
