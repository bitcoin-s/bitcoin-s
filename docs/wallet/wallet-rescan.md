---
title: Wallet Rescans
id: wallet-rescan
---

With [BIP157](https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki) you can cache block filters locally to use
later for rescans in the case you need to restore your wallets. Our [chain](../applications/chain.md) project gives us
an API with the ability to query for filters.

You can rescan your wallet with filters with [`WalletApi.rescanNeutrinoWallet()`](https://github.com/bitcoin-s/bitcoin-s/blob/master/core/src/main/scala/org/bitcoins/core/api/wallet/NeutrinoWalletApi.scala#L77)

### Example

To run this example you need to make sure you have access to a bitcoind binary. You can download this with bitcoin-s by doing
`sbt downloadBitcoind`

```scala mdoc:invisible
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.fixtures._
import org.bitcoins.testkit.wallet._
import org.bitcoins.server.BitcoinSAppConfig
import akka.actor.ActorSystem
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration.DurationInt
```

```scala mdoc:compile-only

//we need an actor system and app config to power this
implicit val system: ActorSystem = ActorSystem(s"wallet-rescan-example")
implicit val ec: ExecutionContext = system.dispatcher
implicit val appConfig: BitcoinSAppConfig = BitcoinSTestAppConfig.getNeutrinoTestConfig()

val bip39PasswordOpt = None
//ok now let's spin up a bitcoind and a bitcoin-s wallet with funds in it
val walletWithBitcoindF = for {
  bitcoind <- BitcoinSFixture.createBitcoindWithFunds()
  walletWithBitcoind <- BitcoinSWalletTest.createWalletWithBitcoindCallbacks(bitcoind, bip39PasswordOpt)
} yield walletWithBitcoind

val walletF = walletWithBitcoindF.map(_.wallet)

val bitcoindF = walletWithBitcoindF.map(_.bitcoind)

//let's see what our initial wallet balance is
val initBalanceF = for {
  w <- walletF
  balance <- w.getBalance()
} yield {
  println(s"Initial wallet balance=${balance}")
  balance
}

//ok great! We have money in the wallet to start,
//now let's delete our internal tables that hold our utxos
//and addresses so that we end up with a 0 balance
val clearedWalletF = for {
  w <- walletF
  _ <- initBalanceF
  clearedWallet <- w.clearAllUtxosAndAddresses()
  zeroBalance <- clearedWallet.getBalance()
} yield {
  println(s"Balance after clearing utxos: ${zeroBalance}")
  clearedWallet
}

//we need to pick how many addresses we want to generate off of our keychain
//when doing a rescan, this means we are generating 100 addrsses
//and then looking for matches. If we find a match, we generate _another_
//100 fresh addresses and search those. We keep doing this until we find
//100 addresses that do not contain a match.
val addrBatchSize = 100
//ok now that we have a cleared wallet, we need to rescan and find our fudns again!
val rescannedBalanceF = for {
  w <- clearedWalletF
  _ <- w.fullRescanNeutrinoWallet(addrBatchSize)
  balanceAfterRescan <- w.getBalance()
} yield {
  println(s"Wallet balance after rescan: ${balanceAfterRescan}")
  ()
}

//cleanup
val cleanupF = for {
  _ <- rescannedBalanceF
  walletWithBitcoind <- walletWithBitcoindF
  _ <- BitcoinSWalletTest.destroyWalletWithBitcoind(walletWithBitcoind)
} yield ()

Await.result(cleanupF, 60.seconds)
```
