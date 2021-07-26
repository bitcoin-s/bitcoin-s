package org.bitcoins.testkit.dlc

import com.typesafe.config.ConfigFactory
import org.bitcoins.dlc.node.DLCNode
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.wallet.BitcoinSWalletTest.destroyDLCWallet
import org.bitcoins.testkit.wallet._
import org.scalatest.FutureOutcome

trait BitcoinSDLCNodeTest extends BitcoinSWalletTest {

  /** Wallet config with data directory set to user temp directory */
  override protected def getFreshConfig: BitcoinSAppConfig = {
    val dlcListen = ConfigFactory.parseString(
      s"""bitcoin-s.dlcnode.listen = "0.0.0.0:${RpcUtil.randomPort}" """)
    BaseWalletTest.getFreshConfig(pgUrl, Vector(dlcListen))
  }

  /** Creates two DLC nodes with wallets that are funded with some bitcoin,
    * these wallets are NOT peered with a bitcoind so the funds in
    * the wallets are not tied to an underlying blockchain.
    */
  def witTwoFundedDLCNodes(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture(
      build = () => {
        val configA = getFreshConfig
        val configB = getFreshConfig
        for {
          walletA <-
            FundWalletUtil.createFundedDLCWallet(
              nodeApi,
              chainQueryApi,
              getBIP39PasswordOpt(),
              Some(segwitWalletConf))(configA, system)
          walletB <- FundWalletUtil.createFundedDLCWallet(
            nodeApi,
            chainQueryApi,
            getBIP39PasswordOpt(),
            Some(segwitWalletConf))(configB, system)

          nodeA = configA.dlcNodeConf.createDLCNode(walletA.wallet)
          nodeB = configB.dlcNodeConf.createDLCNode(walletB.wallet)

          _ <- nodeA.start()
          _ <- nodeB.start()
        } yield (nodeA, nodeB)
      },
      destroy = { nodes: (DLCNode, DLCNode) =>
        for {
          _ <- destroyDLCWallet(nodes._1.wallet.asInstanceOf[DLCWallet])
          _ <- destroyDLCWallet(nodes._2.wallet.asInstanceOf[DLCWallet])
        } yield ()
      }
    )(test)
  }
}
