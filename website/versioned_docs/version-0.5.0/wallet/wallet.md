---
title: Wallet
id: version-0.5.0-wallet
original_id: wallet
---

## Bitcoin-s wallet
Bitcoin-s comes bundled with a rudimentary Bitcoin wallet. This wallet
is capable of managing private keys, generating addresses, constructing
and signing transactions, among other things. It is BIP32/BIP44/BIP49/BIP84
compatible.

This wallet is currently only released as a library, and not as a binary.
This is because it (nor the documentation) is not deemed production
ready. Use at your own risk, and without too much money depending on it.

### How is the bitcoin-s wallet implemented

The bitcoin-s wallet is a scalable way for individuals up to large bitcoin exchanges to safely and securely store their bitcoin in a scalable way.

All key interactions are delegated to the [key-manager](../key-manager/key-manager.md) which is a minimal dependency library to store and use key material.

By default, we store the encrypted root key in `$HOME/.bitcoin-s/seeds/encrypted-bitcoin-s-seed.json`. This is the seed that is used for each of the wallets on each bitcoin network.
Multiple wallet seeds can be saved using the `bitcoin-s.wallet.walletName` config option.
You can read more in the [key manager docs](../key-manager/server-key-manager.md).

The wallet itself is used to manage the utxo life cycle, create transactions, and update wallet balances to show how much money you have the on a bitcoin network.

We use [slick](https://scala-slick.org/doc/3.3.1/) as middleware to support different database types. Depending on your use case, you can use something as simple as sqlite, or something much more scalable like postgres.


### Example

This guide shows how to create a Bitcoin-s wallet and then
peer it with a `bitcoind` instance that relays
information about what is happening on the blockchain
through the P2P network.

This is useful if you want more flexible signing procedures in
the JVM ecosystem and more granular control over your
UTXOs with popular database like Postgres, SQLite, etc.

This code snippet you have a running `bitcoind` instance, locally
on regtest.


```scala
implicit val ec = scala.concurrent.ExecutionContext.global


val config = ConfigFactory.parseString {
    """
    | bitcoin-s {
    |   network = regtest
    | }
    """.stripMargin
}


val datadir = Files.createTempDirectory("bitcoin-s-wallet")


implicit val walletConfig = WalletAppConfig(datadir, config)

// we also need to store chain state for syncing purposes
implicit val chainConfig = ChainAppConfig(datadir, config)

// when this future completes, we have
// created the necessary directories and
// databases for managing both chain state
// and wallet state
val configF: Future[Unit] = for {
    _ <- walletConfig.start()
    _ <- chainConfig.start()
} yield ()

val bitcoindInstance = BitcoindInstance.fromDatadir()

val bitcoind = BitcoindRpcClient(bitcoindInstance)

// when this future completes, we have
// synced our chain handler to our bitcoind
// peer
val syncF: Future[ChainApi] = configF.flatMap { _ =>
    val getBestBlockHashFunc = { () =>
        bitcoind.getBestBlockHash
    }

    
    val getBlockHeaderFunc = { hash: DoubleSha256DigestBE =>
        bitcoind.getBlockHeader(hash).map(_.blockHeader)
    }

    val blockHeaderDAO = BlockHeaderDAO()
    val compactFilterHeaderDAO = CompactFilterHeaderDAO()
    val compactFilterDAO = CompactFilterDAO()
    val chainHandler = ChainHandler(
        blockHeaderDAO,
        compactFilterHeaderDAO,
        compactFilterDAO,
        blockFilterCheckpoints = Map.empty)

    ChainSync.sync(chainHandler, getBlockHeaderFunc, getBestBlockHashFunc)
}

//initialize our key manager, where we store our keys
val aesPasswordOpt = Some(AesPassword.fromString("password"))
//you can add a password here if you want
//val bip39PasswordOpt = Some("my-password-here")
val bip39PasswordOpt = None
val keyManager = BIP39KeyManager.initialize(aesPasswordOpt, walletConfig.kmParams, bip39PasswordOpt).getOrElse {
  throw new RuntimeException(s"Failed to initalize key manager")
}

// once this future completes, we have a initialized
// wallet
val wallet = Wallet(keyManager, new NodeApi {
    override def broadcastTransaction(tx: Transaction): Future[Unit] = Future.successful(())
    override def downloadBlocks(blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = Future.successful(())
  }, new ChainQueryApi {
    override def epochSecondToBlockHeight(time: Long): Future[Int] = Future.successful(0)
    override def getBlockHeight(blockHash: DoubleSha256DigestBE): Future[Option[Int]] = Future.successful(None)
    override def getBestBlockHash(): Future[DoubleSha256DigestBE] = Future.successful(DoubleSha256DigestBE.empty)
    override def getNumberOfConfirmations(blockHashOpt: DoubleSha256DigestBE): Future[Option[Int]] = Future.successful(None)
    override def getFilterCount: Future[Int] = Future.successful(0)
    override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] = Future.successful(0)
    override def getFiltersBetweenHeights(startHeight: Int, endHeight: Int): Future[Vector[FilterResponse]] = Future.successful(Vector.empty)
  }, ConstantFeeRateProvider(SatoshisPerVirtualByte.one), creationTime = Instant.now)
val walletF: Future[WalletApi] = configF.flatMap { _ =>
  Wallet.initialize(wallet,bip39PasswordOpt)
}

// when this future completes, ww have sent a transaction
// from bitcoind to the Bitcoin-S wallet
val transactionF: Future[(Transaction, Option[DoubleSha256DigestBE])] = for {
    wallet <- walletF
    address <- wallet.getNewAddress()
    txid <- bitcoind.sendToAddress(address, 3.bitcoin)
    transaction <- bitcoind.getRawTransaction(txid)
} yield (transaction.hex, transaction.blockhash)

// when this future completes, we have processed
// the transaction from bitcoind, and we have
// queried our balance for the current balance
val balanceF: Future[CurrencyUnit] = for {
    wallet <- walletF
    (tx, blockhash) <- transactionF
    _ <- wallet.processTransaction(tx, blockhash)
    balance <- wallet.getBalance()
} yield balance

balanceF.foreach { balance =>
    println(s"Bitcoin-S wallet balance: $balance")
}
```
