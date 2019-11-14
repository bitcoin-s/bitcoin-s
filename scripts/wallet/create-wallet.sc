import akka.actor.ActorSystem

implicit val system = ActorSystem(s"create-wallet-script-${System.currentTimeMillis()}")
implicit val ec = system.dispatcher

import com.typesafe.config.ConfigFactory
val config = ConfigFactory.parseString {
  """
    | bitcoin-s {
    |   network = regtest
    |   logging {
    |     LEVEL = info
    |   }
    | }
    """.stripMargin
}

import java.nio.file.Files
val datadir = Files.createTempDirectory("bitcoin-s-wallet")

import org.bitcoins.wallet.config.WalletAppConfig
implicit val walletConfig = WalletAppConfig(datadir, config)

// we also need to store chain state for syncing purposes
import org.bitcoins.chain.config.ChainAppConfig
implicit val chainConfig = ChainAppConfig(datadir, config)

// when this future completes, we have
// created the necessary directories and
// databases for managing both chain state
// and wallet state
import scala.concurrent._
val configF: Future[Unit] = for {
  _ <- walletConfig.initialize()
  _ <- chainConfig.initialize()
} yield ()

import org.bitcoins.rpc.config.BitcoindInstance
val bitcoindInstance = BitcoindInstance.fromDatadir()

import org.bitcoins.rpc.client.common.BitcoindRpcClient
val bitcoind = BitcoindRpcClient(bitcoindInstance)

// when this future completes, we have
// synced our chain handler to our bitcoind
// peer
import org.bitcoins.chain.api.ChainApi
val syncF: Future[ChainApi] = configF.flatMap { _ =>
  val getBestBlockHashFunc = { () =>
    bitcoind.getBestBlockHash
  }

  import org.bitcoins.core.crypto.DoubleSha256DigestBE
  val getBlockHeaderFunc = { hash: DoubleSha256DigestBE =>
    bitcoind.getBlockHeader(hash).map(_.blockHeader)
  }


  import org.bitcoins.chain.models._
  import org.bitcoins.chain.blockchain.ChainHandler
  val blockHeaderDAO = BlockHeaderDAO()
  val compactFilterHeaderDAO = CompactFilterHeaderDAO()
  val compactFilterDAO = CompactFilterDAO()
  val chainHandler = ChainHandler(
    blockHeaderDAO,
    compactFilterHeaderDAO,
    compactFilterDAO,
    blockchains = Vector.empty,
    blockFilterCheckpoints = Map.empty)

  import org.bitcoins.chain.blockchain.sync.ChainSync
  ChainSync.sync(chainHandler, getBlockHeaderFunc, getBestBlockHashFunc)
}

// once this future completes, we have a initialized
// wallet
import org.bitcoins.wallet.api.LockedWalletApi
import org.bitcoins.wallet.api.InitializeWalletSuccess
import org.bitcoins.wallet.Wallet
val walletF: Future[LockedWalletApi] = configF.flatMap { _ =>
  Wallet.initialize().collect {
    case InitializeWalletSuccess(wallet) => wallet

  }
}


// when this future completes, ww have sent a transaction
// from bitcoind to the Bitcoin-S wallet
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.currency._
val transactionF: Future[Transaction] = for {
  wallet <- walletF
  startBalance <- wallet.getBalance()
  address <- wallet.getNewAddress()
  txid <- bitcoind.sendToAddress(address, 3.bitcoin)
  transaction <- bitcoind.getRawTransaction(txid)
} yield {
  println(s"bitcoin-s wallet starting balance=${startBalance}")
  transaction.hex
}

// when this future completes, we have processed
// the transaction from bitcoind, and we have
// queried our balance for the current balance
val balanceF: Future[CurrencyUnit] = for {
  wallet <- walletF
  tx <- transactionF
  _ <- wallet.processTransaction(tx, confirmations = 0)
  balance <- wallet.getBalance
} yield balance

balanceF.foreach { balance =>
  println(s"bitcoin-s wallet balance: $balance")
  system.terminate()
}