package org.bitcoins.node

import org.bitcoins.core.currency._
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.scalatest.FutureOutcome
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.Future
import org.bitcoins.node.networking.peer.DataMessageHandler

class NodeWithWalletTest extends BitcoinSWalletTest {

  override type FixtureParam = WalletWithBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNewWalletAndBitcoind(test)

  it must "load a bloom filter and receive information about received payments" in {
    param =>
      val WalletWithBitcoind(wallet, rpc) = param

      implicit val walletConfig: WalletAppConfig = WalletAppConfig()
      implicit val nodeConfig: NodeAppConfig = NodeAppConfig()
      implicit val chainConfig: ChainAppConfig = ChainAppConfig()

      val chainHandler = {
        val bhDao = BlockHeaderDAO(chainConfig)
        ChainHandler(bhDao, chainConfig)
      }

      val peer = Peer.fromBitcoind(rpc.instance)

      val callbacks = SpvNodeCallbacks(
        onBlockReceived = { block =>
          logger.error(s"Received a block")
          // for some reason this isn't triggered?
          fail(s"Received a block! We are only expecting merkle blocks")
        },
        onMerkleBlockReceived =
          block => logger.info(s"Received merkle block: $block"),
        onTxReceived = tx => logger.info(s"Received TX: $tx"),
      )

      val spv = SpvNode(peer, chainHandler, callbacks = callbacks)

      logger.info(
        s"Bitcoind instance has datadir: ${rpc.instance.authCredentials.datadir}")

      for {
        _ <- nodeConfig.initialize()
        _ = logger.info(s"Node config initialized")

        address <- wallet.getNewAddress()
        info <- wallet.getAddressInfo(address).map {
          case Some(info) => info
          case None       => fail(s"Didn't get address info")
        }
        _ <- spv.addPubKey(info.pubkey)
        _ = logger.info(s"Added pubkey from wallet to SPV node")

        started <- spv.start()
        bloom = started.bloomFilter match {
          case Some(b) => b
          case None    => fail(s"Wallet had no bloom filter")
        }

        _ = assert(bloom.contains(info.pubkey.bytes))
        _ = logger.info(s"Bloom matches pubkey")

        _ = logger.info(s"SPV node started")
        txid <- rpc.sendToAddress(address, 1.bitcoin)
        _ = logger.info(s"Sent money to wallet address")

        rpcAddress <- rpc.getNewAddress
        _ = logger.info(s"About to generate a block")
        block +: _ <- rpc.generateToAddress(blocks = 1, rpcAddress)
        txInfo <- rpc.getTransaction(txid)
        _ = assert(txInfo.confirmations > 0 && txInfo.blockhash.contains(block))
        _ = logger.info(s"Generated a block: ${block}")
        balance <- wallet.getBalance()
        _ = logger.info(s"wallet balance: $balance")

        _ <- Future {
          // this is to make the test hang, so you have time to investigate logs and whatnot
          Thread.sleep(100000)
        }
      } yield fail

  }
}
