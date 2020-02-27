package org.bitcoins.testkit.wallet

import akka.actor.ActorSystem
import com.typesafe.config.Config
import org.bitcoins.core.api.{ChainQueryApi, NodeApi}
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, CurrencyUnits, _}
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.util.TransactionTestUtil
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.bitcoins.wallet.Wallet

import scala.concurrent.{ExecutionContext, Future}

trait FundWalletUtil {

  def fundAccountForWallet(
      amts: Vector[CurrencyUnit],
      account: HDAccount,
      wallet: Wallet)(implicit ec: ExecutionContext): Future[Wallet] = {

    val init = Future.successful(Vector.empty[BitcoinAddress])
    val addressesF: Future[Vector[BitcoinAddress]] = 0.until(3).foldLeft(init) {
      case (accumF, _) =>
        //this Thread.sleep is needed because of
        //https://github.com/bitcoin-s/bitcoin-s/issues/1009
        //once that is resolved we should be able to remove this
        for {
          accum <- accumF
          address <- wallet.getNewAddress(account)
        } yield {
          accum.:+(address)
        }
    }

    //construct three txs that send money to these addresses
    //these are "fictional" transactions in the sense that the
    //outpoints do not exist on a blockchain anywhere
    val txsF = for {
      addresses <- addressesF
    } yield {
      addresses.zip(amts).map {
        case (addr, amt) =>
          val output =
            TransactionOutput(value = amt, scriptPubKey = addr.scriptPubKey)
          TransactionTestUtil.buildTransactionTo(output)
      }
    }

    val fundedWalletF =
      txsF.flatMap(txs => wallet.processTransactions(txs, None))

    fundedWalletF.map(_.asInstanceOf[Wallet])
  }

  /** Funds a bitcoin-s wallet with 3 utxos with 1, 2 and 3 bitcoin in the utxos */
  def fundWallet(wallet: Wallet)(
      implicit ec: ExecutionContext): Future[FundedWallet] = {

    val defaultAcctAmts = Vector(1.bitcoin, 2.bitcoin, 3.bitcoin)
    val expectedDefaultAmt = defaultAcctAmts.fold(CurrencyUnits.zero)(_ + _)
    val account1Amt = Vector(Bitcoins(0.2), Bitcoins(0.3), Bitcoins(0.5))
    val expectedAccount1Amt = account1Amt.fold(CurrencyUnits.zero)(_ + _)

    val defaultAccount = wallet.walletConfig.defaultAccount
    val fundedDefaultAccountWalletF = FundWalletUtil.fundAccountForWallet(
      amts = defaultAcctAmts,
      account = defaultAccount,
      wallet = wallet
    )

    val hdAccount1 = WalletTestUtil.getHdAccount1(wallet.walletConfig)
    val fundedAccount1WalletF = for {
      fundedDefaultAcct <- fundedDefaultAccountWalletF
      fundedAcct1 <- FundWalletUtil.fundAccountForWallet(
        amts = account1Amt,
        account = hdAccount1,
        fundedDefaultAcct
      )
    } yield fundedAcct1

    //sanity check to make sure we have money
    for {
      fundedWallet <- fundedAccount1WalletF
      balance <- fundedWallet.getBalance(defaultAccount)
      _ = require(
        balance == expectedDefaultAmt,
        s"Funding wallet fixture failed to fund the wallet, got balance=${balance} expected=${expectedDefaultAmt}")

      account1Balance <- fundedWallet.getBalance(hdAccount1)
      _ = require(
        account1Balance == expectedAccount1Amt,
        s"Funding wallet fixture failed to fund account 1, " +
          s"got balance=${hdAccount1} expected=${expectedAccount1Amt}"
      )

    } yield FundedWallet(fundedWallet)
  }
}

object FundWalletUtil extends FundWalletUtil {

  /** This is a wallet that was two funded accounts
    * Account 0 (default account) has utxos of 1,2,3 bitcoin in it (6 btc total)
    * Account 1 has a utxos of 0.2,0.3,0.5 bitcoin in it (0.6 total)
    * */
  case class FundedWallet(wallet: Wallet)

  /** This creates a wallet that was two funded accounts
    * Account 0 (default account) has utxos of 1,2,3 bitcoin in it (6 btc total)
    * Account 1 has a utxos of 0.2,0.3,0.5 bitcoin in it (1 btc total)
    * */
  def createFundedWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      extraConfig: Option[Config] = None)(
      implicit config: BitcoinSAppConfig,
      system: ActorSystem): Future[FundedWallet] = {

    import system.dispatcher
    for {
      wallet <- BitcoinSWalletTest.createWallet2Accounts(nodeApi,
                                                         chainQueryApi,
                                                         extraConfig)
      funded <- FundWalletUtil.fundWallet(wallet)
    } yield funded
  }
}
