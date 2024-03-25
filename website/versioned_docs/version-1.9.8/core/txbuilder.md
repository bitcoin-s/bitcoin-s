---
id: version-1.9.8-txbuilder
title: TxBuilder Example
original_id: txbuilder
---

Bitcoin-S features a transaction building API that allows you to construct and sign Bitcoin transactions. Here's an example of how to use it


```scala
implicit val ec: ExecutionContext = ExecutionContext.Implicits.global
// ec: ExecutionContext = scala.concurrent.impl.ExecutionContextImpl$$anon$3@7c906c98[Running, parallelism = 16, size = 0, active = 0, running = 0, steals = 2799, tasks = 0, submissions = 0]

// Initialize a transaction builder
val builder = RawTxBuilder()
// builder: RawTxBuilder = RawTxBuilder()

// generate a fresh private key that we are going to use in the scriptpubkey
val privKey = ECPrivateKey.freshPrivateKey
// privKey: ECPrivateKey = Masked(ECPrivateKey)
val pubKey = privKey.publicKey
// pubKey: ECPublicKey = ECPublicKey(02f6ec180dbc3588690aa3bce864f86e54ec289821342b090271488361841f8dd9)

// this is the script that the TxBuilder is going to create a
// script signature that validly spends this scriptPubKey
val creditingSpk = P2PKHScriptPubKey(pubKey = privKey.publicKey)
// creditingSpk: P2PKHScriptPubKey = pkh(241ad4894d3d6ed55d6dee9087a13cef8bbe3fb6)
val amount = 10000.satoshis
// amount: Satoshis = 10000 sats

// this is the UTXO we are going to be spending
val utxo =
  TransactionOutput(value = amount, scriptPubKey = creditingSpk)
// utxo: TransactionOutput = TransactionOutput(10000 sats,pkh(241ad4894d3d6ed55d6dee9087a13cef8bbe3fb6))

// the private key that locks the funds for the script we are spending too
val destinationPrivKey = ECPrivateKey.freshPrivateKey
// destinationPrivKey: ECPrivateKey = Masked(ECPrivateKey)

// the amount we are sending -- 5000 satoshis -- to the destinationSPK
val destinationAmount = 5000.satoshis
// destinationAmount: Satoshis = 5000 sats

// the script that corresponds to destination private key, this is what is receiving the money
val destinationSPK =
  P2PKHScriptPubKey(pubKey = destinationPrivKey.publicKey)
// destinationSPK: P2PKHScriptPubKey = pkh(5bc530a9388a51ba4b65d229246e579dcbc83f3c)

// this is where we are sending money too
// we could add more destinations here if we
// wanted to batch transactions
val destinations = {
    val destination0 = TransactionOutput(value = destinationAmount,
                                         scriptPubKey = destinationSPK)

    Vector(destination0)
}
// destinations: Vector[TransactionOutput] = Vector(TransactionOutput(5000 sats,pkh(5bc530a9388a51ba4b65d229246e579dcbc83f3c)))

// Add the destinations to the tx builder
builder ++= destinations
// res0: RawTxBuilder = RawTxBuilder()

// we have to fabricate a transaction that contains the
// UTXO we are trying to spend. If this were a real blockchain
// we would need to reference the UTXO set
val creditingTx = BaseTransaction(version = Int32.one,
                                  inputs = Vector.empty,
                                  outputs = Vector(utxo),
                                  lockTime = UInt32.zero)
// creditingTx: BaseTransaction = BaseTransaction(Int32Impl(1),Vector(),Vector(TransactionOutput(10000 sats,pkh(241ad4894d3d6ed55d6dee9087a13cef8bbe3fb6))),UInt32Impl(0))

// this is the information we need from the crediting TX
// to properly "link" it in the transaction we are creating
val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
// outPoint: TransactionOutPoint = TransactionOutPoint(da61ba5a6f0fc0119e42f8887d55999352ecc6438a0e17f3081c434553d7c141:0)
val input = TransactionInput(
    outPoint,
    EmptyScriptSignature,
    sequenceNumber = UInt32.zero)
// input: TransactionInput = TransactionInputImpl(TransactionOutPoint(da61ba5a6f0fc0119e42f8887d55999352ecc6438a0e17f3081c434553d7c141:0),EmptyScriptSignature,UInt32Impl(0))

// Add a new input to our builder
builder += input
// res1: RawTxBuilder = RawTxBuilder()

// We can now generate a RawTxBuilderResult ready to be finalized
val builderResult = builder.result()
// builderResult: RawTxBuilderResult = RawTxBuilderResult(Int32Impl(2),Vector(TransactionInputImpl(TransactionOutPoint(da61ba5a6f0fc0119e42f8887d55999352ecc6438a0e17f3081c434553d7c141:0),EmptyScriptSignature,UInt32Impl(0))),Vector(TransactionOutput(5000 sats,pkh(5bc530a9388a51ba4b65d229246e579dcbc83f3c))),UInt32Impl(0))

// this contains the information needed to analyze our input during finalization
val inputInfo = P2PKHInputInfo(outPoint, amount, privKey.publicKey)
// inputInfo: P2PKHInputInfo = P2PKHInputInfo(TransactionOutPoint(da61ba5a6f0fc0119e42f8887d55999352ecc6438a0e17f3081c434553d7c141:0),10000 sats,ECPublicKey(02f6ec180dbc3588690aa3bce864f86e54ec289821342b090271488361841f8dd9))

// this is how much we are going to pay as a fee to the network
// for this example, we are going to pay 1 satoshi per byte
val feeRate = SatoshisPerByte(1.satoshi)
// feeRate: SatoshisPerByte = 1 sats/byte

val changePrivKey = ECPrivateKey.freshPrivateKey
// changePrivKey: ECPrivateKey = Masked(ECPrivateKey)
val changeSPK = P2PKHScriptPubKey(pubKey = changePrivKey.publicKey)
// changeSPK: P2PKHScriptPubKey = pkh(3f4df7d840aee9abbe14e8f64d0677ccbb881342)

// We chose a finalizer that adds a change output to our tx based on a fee rate
val finalizer = StandardNonInteractiveFinalizer(
    Vector(inputInfo),
    feeRate,
    changeSPK)
// finalizer: StandardNonInteractiveFinalizer = StandardNonInteractiveFinalizer(Vector(P2PKHInputInfo(TransactionOutPoint(da61ba5a6f0fc0119e42f8887d55999352ecc6438a0e17f3081c434553d7c141:0),10000 sats,ECPublicKey(02f6ec180dbc3588690aa3bce864f86e54ec289821342b090271488361841f8dd9))),1 sats/byte,pkh(3f4df7d840aee9abbe14e8f64d0677ccbb881342))

// We can now finalize the tx builder result from earlier with this finalizer
val unsignedTx: Transaction = finalizer.buildTx(builderResult)
// unsignedTx: Transaction = BaseTransaction(Int32Impl(2),Vector(TransactionInputImpl(TransactionOutPoint(da61ba5a6f0fc0119e42f8887d55999352ecc6438a0e17f3081c434553d7c141:0),EmptyScriptSignature,UInt32Impl(0))),Vector(TransactionOutput(5000 sats,pkh(5bc530a9388a51ba4b65d229246e579dcbc83f3c)), TransactionOutput(4775 sats,pkh(3f4df7d840aee9abbe14e8f64d0677ccbb881342))),UInt32Impl(0))

// We now turn to signing the unsigned transaction
// this contains all the information we need to
// validly sign the UTXO above
val utxoInfo = ScriptSignatureParams(inputInfo = inputInfo,
                                     prevTransaction = creditingTx,
                                     signers = Vector(privKey),
                                     hashType =
                                         HashType.sigHashAll)
// utxoInfo: ScriptSignatureParams[P2PKHInputInfo] = ScriptSignatureParams(P2PKHInputInfo(TransactionOutPoint(da61ba5a6f0fc0119e42f8887d55999352ecc6438a0e17f3081c434553d7c141:0),10000 sats,ECPublicKey(02f6ec180dbc3588690aa3bce864f86e54ec289821342b090271488361841f8dd9)),BaseTransaction(Int32Impl(1),Vector(),Vector(TransactionOutput(10000 sats,pkh(241ad4894d3d6ed55d6dee9087a13cef8bbe3fb6))),UInt32Impl(0)),Vector(Masked(ECPrivateKey)),SIGHASH_ALL(1))

// all of the UTXO spending information, since we only have
// one input, this is just one element
val utxoInfos: Vector[ScriptSignatureParams[InputInfo]] = Vector(utxoInfo)
// utxoInfos: Vector[ScriptSignatureParams[InputInfo]] = Vector(ScriptSignatureParams(P2PKHInputInfo(TransactionOutPoint(da61ba5a6f0fc0119e42f8887d55999352ecc6438a0e17f3081c434553d7c141:0),10000 sats,ECPublicKey(02f6ec180dbc3588690aa3bce864f86e54ec289821342b090271488361841f8dd9)),BaseTransaction(Int32Impl(1),Vector(),Vector(TransactionOutput(10000 sats,pkh(241ad4894d3d6ed55d6dee9087a13cef8bbe3fb6))),UInt32Impl(0)),Vector(Masked(ECPrivateKey)),SIGHASH_ALL(1)))

// Yay! Now we use the RawTxSigner object to sign the tx.
// The 'sign' method is going produce a validly signed transaction
// This is going to iterate through each of the UTXOs and use
// the corresponding ScriptSignatureParams to produce a validly
// signed input. This UTXO has:
//   1: one input
//   2: outputs (destination and change outputs)
//   3: a fee rate of 1 satoshi/byte
val signedTx: Transaction =
  RawTxSigner.sign(
      utx = unsignedTx,
      utxoInfos = utxoInfos,
      expectedFeeRate = feeRate
  )
// signedTx: Transaction = BaseTransaction(Int32Impl(2),Vector(TransactionInputImpl(TransactionOutPoint(da61ba5a6f0fc0119e42f8887d55999352ecc6438a0e17f3081c434553d7c141:0),P2PKHScriptSignature(ECPublicKeyBytes(ByteVector(33 bytes, 0x02f6ec180dbc3588690aa3bce864f86e54ec289821342b090271488361841f8dd9)), ECDigitalSignature(304402201e30e76d8cc7cbb665bb06bca30727bd5ab2f6a1091b43025203c720d4d33b72022005e231de0e476d4d8333e78cab1d792fa08f889cc7d7c593add104b099dfb72f01)),UInt32Impl(0))),Vector(TransactionOutput(5000 sats,pkh(5bc530a9388a51ba4b65d229246e579dcbc83f3c)), TransactionOutput(4775 sats,pkh(3f4df7d840aee9abbe14e8f64d0677ccbb881342))),UInt32Impl(0))
```

```scala
signedTx.inputs.length
// res2: Int = 1

signedTx.outputs.length
// res3: Int = 2

//remember, you can call .hex on any bitcoin-s data structure to get the hex representation!
signedTx.hex
// res4: String = 020000000141c1d75345431c08f3170e8a43c6ec529399557d88f8429e11c00f6f5aba61da000000006a47304402201e30e76d8cc7cbb665bb06bca30727bd5ab2f6a1091b43025203c720d4d33b72022005e231de0e476d4d8333e78cab1d792fa08f889cc7d7c593add104b099dfb72f012102f6ec180dbc3588690aa3bce864f86e54ec289821342b090271488361841f8dd9000000000288130000000000001976a9145bc530a9388a51ba4b65d229246e579dcbc83f3c88aca7120000000000001976a9143f4df7d840aee9abbe14e8f64d0677ccbb88134288ac00000000
```
