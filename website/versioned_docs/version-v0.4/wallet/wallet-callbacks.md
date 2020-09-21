---
title: Wallet Callbacks
id: version-v0.4-wallet-callbacks
original_id: wallet-callbacks
---

#### Callbacks

Bitcoin-S support call backs for the following events that happen in the wallet:

1. onTransactionProcessed
2. onTransactionBroadcast
3. onReservedUtxos
4. onNewAddressGenerated

That means every time one of these events happens, we will call your callback
so that you can be notified of the event. These callbacks will be run after the message has been
recieved and will execute synchronously. If any of them fail an error log will be output, and the remainder of the callbacks will continue.
Let's make an easy one:

#### Example

Here is an example of constructing a wallet and registering a callback, so you can be notified of an event.


```scala
implicit val system: ActorSystem = ActorSystem("example")
implicit val ec: ExecutionContextExecutor = system.dispatcher
implicit val walletConf: WalletAppConfig =
    BitcoinSTestAppConfig.getNeutrinoTestConfig().walletConf

// let's use a helper method to get a v19 bitcoind
// and a ChainApi
val bitcoind = BitcoindV19RpcClient(BitcoindInstance.fromConfigFile())

// Create our key manager
  val keyManagerE = BIP39KeyManager.initialize(kmParams = walletConf.kmParams,
                                               bip39PasswordOpt = None)

val keyManager = keyManagerE match {
    case Right(keyManager) => keyManager
    case Left(err) =>
      throw new RuntimeException(s"Cannot initialize key manager err=$err")
  }

// Here is a super simple example of a callback, this could be replaced with anything, from
// relaying the transaction on the network, finding relevant wallet outputs, verifying the transaction,
// or writing it to disk
val exampleProcessTx: OnTransactionProcessed = (tx: Transaction) =>
    Future.successful(println(s"Processed Tx: ${tx.txIdBE}"))

// Create our WalletCallbacks that
val exampleCallbacks = WalletCallbacks(
    onTransactionProcessed = Vector(exampleProcessTx))

// Now we can create a wallet
val wallet =
    Wallet(keyManager = keyManager,
           nodeApi = bitcoind,
           chainQueryApi = bitcoind,
           feeRateApi = ConstantFeeRateProvider(SatoshisPerVirtualByte.one),
           creationTime = Instant.now)

// Finally, we can add the callbacks to our wallet config
walletConf.addCallbacks(exampleCallbacks)

// Then to trigger the event we can run
val exampleTx = Transaction(
    "0200000000010258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd7500000000da00473044022074018ad4180097b873323c0015720b3684cc8123891048e7dbcd9b55ad679c99022073d369b740e3eb53dcefa33823c8070514ca55a7dd9544f157c167913261118c01483045022100f61038b308dc1da865a34852746f015772934208c6d24454393cd99bdf2217770220056e675a675a6d0a02b85b14e5e29074d8a25a9b5760bea2816f661910a006ea01475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752aeffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d01000000232200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b2028903ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f000400473044022062eb7a556107a7c73f45ac4ab5a1dddf6f7075fb1275969a7f383efff784bcb202200c05dbb7470dbf2f08557dd356c7325c1ed30913e996cd3840945db12228da5f01473044022065f45ba5998b59a27ffe1a7bed016af1f1f90d54b3aa8f7450aa5f56a25103bd02207f724703ad1edb96680b284b56d4ffcb88f7fb759eabbe08aa30f29b851383d20147522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae00000000")
wallet.processTransaction(exampleTx, None)
```