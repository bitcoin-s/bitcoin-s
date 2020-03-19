---
id: version-0.3.0-txbuilder
title: TxBuilder Example
original_id: txbuilder
---

Bitcoin-S features a transaction builder that constructs and signs Bitcoin
transactions. Here's an example of how to use it


```scala
implicit val ec: ExecutionContext = ExecutionContext.Implicits.global
// ec: ExecutionContext = scala.concurrent.impl.ExecutionContextImpl$$anon$3@259cf018[Running, parallelism = 8, size = 2, active = 0, running = 0, steals = 20, tasks = 0, submissions = 0]

// generate a fresh private key that we are going to use in the scriptpubkey
val privKey = ECPrivateKey.freshPrivateKey
// privKey: ECPrivateKey = Masked(ECPrivateKeyImpl)
val pubKey = privKey.publicKey
// pubKey: ECPublicKey = ECPublicKey(0383604b72577aebd92ab56ba6141a3e942fb86941e37b2e978b11257f8b9a6400)

// this is the script that the TxBuilder is going to create a
// script signature that validly spends this scriptPubKey
val creditingSpk = P2PKHScriptPubKey(pubKey = privKey.publicKey)
// creditingSpk: P2PKHScriptPubKey = pkh(be30549d4ac6b3391370321ffbc630aef84272e6)
val amount = 10000.satoshis
// amount: Satoshis = 10000 sats

// this is the UTXO we are going to be spending
val utxo =
  TransactionOutput(value = amount, scriptPubKey = creditingSpk)
// utxo: TransactionOutput = TransactionOutput(10000 sats,pkh(be30549d4ac6b3391370321ffbc630aef84272e6))

// the private key that locks the funds for the script we are spending too
val destinationPrivKey = ECPrivateKey.freshPrivateKey
// destinationPrivKey: ECPrivateKey = Masked(ECPrivateKeyImpl)

// the amount we are sending -- 5000 satoshis -- to the destinationSPK
val destinationAmount = 5000.satoshis
// destinationAmount: Satoshis = 5000 sats

// the script that corresponds to destination private key, this is what is protecting the money
val destinationSPK =
  P2PKHScriptPubKey(pubKey = destinationPrivKey.publicKey)
// destinationSPK: P2PKHScriptPubKey = pkh(fab4c315dedacdf530c6b560c146514749533e83)

// this is where we are sending money too
// we could add more destinations here if we
// wanted to batch transactions
val destinations = {
    val destination1 = TransactionOutput(value = destinationAmount,
                                         scriptPubKey = destinationSPK)

    Vector(destination1)
}
// destinations: Vector[TransactionOutput] = Vector(TransactionOutput(5000 sats,pkh(fab4c315dedacdf530c6b560c146514749533e83)))

// we have to fabricate a transaction that contains the
// UTXO we are trying to spend. If this were a real blockchain
// we would need to reference the UTXO set
val creditingTx = BaseTransaction(version = Int32.one,
                                  inputs = Vector.empty,
                                  outputs = Vector(utxo),
                                  lockTime = UInt32.zero)
// creditingTx: BaseTransaction = BaseTransactionImpl(Int32Impl(1),Vector(),Vector(TransactionOutput(10000 sats,pkh(be30549d4ac6b3391370321ffbc630aef84272e6))),UInt32Impl(0))

// this is the information we need from the crediting TX
// to properly "link" it in the transaction we are creating
val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
// outPoint: TransactionOutPoint = TransactionOutPoint(0f9997fb129243db98adaf0fb4882c14cfbda0a076b954b4bd0a7f500a41eeff:0)

// this contains all the information we need to
// validly sign the UTXO above
val utxoSpendingInfo = BitcoinUTXOSpendingInfoFull(outPoint = outPoint,
                                                   output = utxo,
                                                   signers = Vector(privKey),
                                                   redeemScriptOpt = None,
                                                   scriptWitnessOpt = None,
                                                   hashType =
                                                       HashType.sigHashAll,
                                                   conditionalPath =
                                                       ConditionalPath.NoConditionsLeft)
// utxoSpendingInfo: BitcoinUTXOSpendingInfoFull = P2PKHSpendingInfo(TransactionOutPoint(0f9997fb129243db98adaf0fb4882c14cfbda0a076b954b4bd0a7f500a41eeff:0),10000 sats,pkh(be30549d4ac6b3391370321ffbc630aef84272e6),Masked(ECPrivateKeyImpl),SIGHASH_ALL(Int32Impl(1)))

// all of the UTXO spending information, since we are only
//spending one UTXO, this is just one element
val utxos: Vector[BitcoinUTXOSpendingInfoFull] = Vector(utxoSpendingInfo)
// utxos: Vector[BitcoinUTXOSpendingInfoFull] = Vector(P2PKHSpendingInfo(TransactionOutPoint(0f9997fb129243db98adaf0fb4882c14cfbda0a076b954b4bd0a7f500a41eeff:0),10000 sats,pkh(be30549d4ac6b3391370321ffbc630aef84272e6),Masked(ECPrivateKeyImpl),SIGHASH_ALL(Int32Impl(1))))

// this is how much we are going to pay as a fee to the network
// for this example, we are going to pay 1 satoshi per byte
val feeRate = SatoshisPerByte(1.satoshi)
// feeRate: SatoshisPerByte = SatoshisPerByte(1 sat)

val changePrivKey = ECPrivateKey.freshPrivateKey
// changePrivKey: ECPrivateKey = Masked(ECPrivateKeyImpl)
val changeSPK = P2PKHScriptPubKey(pubKey = changePrivKey.publicKey)
// changeSPK: P2PKHScriptPubKey = pkh(79c2ad65b2805f0b56ee83b160a0fc6e9eeec5ff)

// the network we are on, for this example we are using
// the regression test network. This is a network you control
// on your own machine
val networkParams = RegTest
// networkParams: RegTest.type = RegTest

// Yay! Now we have a TxBuilder object that we can use
// to sign the TX.
val txBuilder: BitcoinTxBuilder = {
  val builderF = BitcoinTxBuilder(
    destinations = destinations,
    utxos = utxos,
    feeRate = feeRate,
    changeSPK = changeSPK,
    network = networkParams)
  Await.result(builderF, 30.seconds)
}
// txBuilder: BitcoinTxBuilder = BitcoinTxBuilderImpl(Vector(TransactionOutput(5000 sats,pkh(fab4c315dedacdf530c6b560c146514749533e83))),Map(TransactionOutPoint(0f9997fb129243db98adaf0fb4882c14cfbda0a076b954b4bd0a7f500a41eeff:0) -> P2PKHSpendingInfo(TransactionOutPoint(0f9997fb129243db98adaf0fb4882c14cfbda0a076b954b4bd0a7f500a41eeff:0),10000 sats,pkh(be30549d4ac6b3391370321ffbc630aef84272e6),Masked(ECPrivateKeyImpl),SIGHASH_ALL(Int32Impl(1)))),SatoshisPerByte(1 sat),pkh(79c2ad65b2805f0b56ee83b160a0fc6e9eeec5ff),RegTest)

// Let's finally produce a validly signed tx!
// The 'sign' method is going produce a validly signed transaction
// This is going to iterate through each of the UTXOs and use
// the corresponding UTXOSpendingInfo to produce a validly
// signed input. This UTXO has:
//   1: one input
//   2: outputs (destination and change outputs)
//   3: a fee rate of 1 satoshi/byte
val signedTx: Transaction = {
  val signF = txBuilder.sign
  Await.result(signF, 30.seconds)
}
// signedTx: Transaction = BaseTransactionImpl(Int32Impl(2),List(TransactionInputImpl(TransactionOutPoint(0f9997fb129243db98adaf0fb4882c14cfbda0a076b954b4bd0a7f500a41eeff:0),P2PKHScriptSignature(ECPublicKey(0383604b72577aebd92ab56ba6141a3e942fb86941e37b2e978b11257f8b9a6400), ECDigitalSignature(3045022100f8138b17824e063daa266388d28117e122bcaca623b5cd5410959b96692d7e5f02201110005441e352ea076ec27c21524ad51ac1b7315209e9760ae07aaf726978ac01)),UInt32Impl(0))),Vector(TransactionOutput(5000 sats,pkh(fab4c315dedacdf530c6b560c146514749533e83)), TransactionOutput(4774 sats,pkh(79c2ad65b2805f0b56ee83b160a0fc6e9eeec5ff))),UInt32Impl(0))
```

```scala
signedTx.inputs.length
// res0: Int = 1

signedTx.outputs.length
// res1: Int = 2

//remember, you can call .hex on any bitcoin-s data structure to get the hex representation!
signedTx.hex
// res2: String = 0200000001ffee410a507f0abdb454b976a0a0bdcf142c88b40fafad98db439212fb97990f000000006b483045022100f8138b17824e063daa266388d28117e122bcaca623b5cd5410959b96692d7e5f02201110005441e352ea076ec27c21524ad51ac1b7315209e9760ae07aaf726978ac01210383604b72577aebd92ab56ba6141a3e942fb86941e37b2e978b11257f8b9a6400000000000288130000000000001976a914fab4c315dedacdf530c6b560c146514749533e8388aca6120000000000001976a91479c2ad65b2805f0b56ee83b160a0fc6e9eeec5ff88ac00000000
```
