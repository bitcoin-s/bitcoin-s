---
id: version-v0.4-txbuilder
title: TxBuilder Example
original_id: txbuilder
---

Bitcoin-S features a transaction building API that allows you to construct and sign Bitcoin transactions. Here's an example of how to use it


```scala
implicit val ec: ExecutionContext = ExecutionContext.Implicits.global
// ec: ExecutionContext = scala.concurrent.impl.ExecutionContextImpl$$anon$3@68dbe23a[Running, parallelism = 12, size = 3, active = 0, running = 0, steals = 3, tasks = 0, submissions = 0]

// Initialize a transaction builder
val builder = RawTxBuilder()
// builder: RawTxBuilder = RawTxBuilder()

// generate a fresh private key that we are going to use in the scriptpubkey
val privKey = ECPrivateKey.freshPrivateKey
// privKey: ECPrivateKey = Masked(ECPrivateKeyImpl)
val pubKey = privKey.publicKey
// pubKey: ECPublicKey = ECPublicKey(037ad941b7ac16fba77a8f76b111e6254f4645116ed60d742e7641c41541231208)

// this is the script that the TxBuilder is going to create a
// script signature that validly spends this scriptPubKey
val creditingSpk = P2PKHScriptPubKey(pubKey = privKey.publicKey)
// creditingSpk: P2PKHScriptPubKey = pkh(6d52a3bf4776ae367dee519bda02a3f6b03fad7a)
val amount = 10000.satoshis
// amount: Satoshis = 10000 sats

// this is the UTXO we are going to be spending
val utxo =
  TransactionOutput(value = amount, scriptPubKey = creditingSpk)
// utxo: TransactionOutput = TransactionOutput(10000 sats,pkh(6d52a3bf4776ae367dee519bda02a3f6b03fad7a))

// the private key that locks the funds for the script we are spending too
val destinationPrivKey = ECPrivateKey.freshPrivateKey
// destinationPrivKey: ECPrivateKey = Masked(ECPrivateKeyImpl)

// the amount we are sending -- 5000 satoshis -- to the destinationSPK
val destinationAmount = 5000.satoshis
// destinationAmount: Satoshis = 5000 sats

// the script that corresponds to destination private key, this is what is receiving the money
val destinationSPK =
  P2PKHScriptPubKey(pubKey = destinationPrivKey.publicKey)
// destinationSPK: P2PKHScriptPubKey = pkh(5223486f5cd3b5a8e7b210fd11ac6212f774f204)

// this is where we are sending money too
// we could add more destinations here if we
// wanted to batch transactions
val destinations = {
    val destination0 = TransactionOutput(value = destinationAmount,
                                         scriptPubKey = destinationSPK)

    Vector(destination0)
}
// destinations: Vector[TransactionOutput] = Vector(TransactionOutput(5000 sats,pkh(5223486f5cd3b5a8e7b210fd11ac6212f774f204)))

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
// creditingTx: BaseTransaction = BaseTransaction(Int32Impl(1),Vector(),Vector(TransactionOutput(10000 sats,pkh(6d52a3bf4776ae367dee519bda02a3f6b03fad7a))),UInt32Impl(0))

// this is the information we need from the crediting TX
// to properly "link" it in the transaction we are creating
val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
// outPoint: TransactionOutPoint = TransactionOutPoint(76aca317ebbd73347d8c95c9af482c7670cb8e7a9efe383a2909e7eb7dd98ea1:0)
val input = TransactionInput(
    outPoint,
    EmptyScriptSignature,
    sequenceNumber = UInt32.zero)
// input: TransactionInput = TransactionInputImpl(TransactionOutPoint(76aca317ebbd73347d8c95c9af482c7670cb8e7a9efe383a2909e7eb7dd98ea1:0),EmptyScriptSignature,UInt32Impl(0))

// Add a new input to our builder
builder += input
// res1: RawTxBuilder = RawTxBuilder()

// We can now generate a RawTxBuilderResult ready to be finalized
val builderResult = builder.result()
// builderResult: RawTxBuilderResult = RawTxBuilderResult(Int32Impl(2),Vector(TransactionInputImpl(TransactionOutPoint(76aca317ebbd73347d8c95c9af482c7670cb8e7a9efe383a2909e7eb7dd98ea1:0),EmptyScriptSignature,UInt32Impl(0))),Vector(TransactionOutput(5000 sats,pkh(5223486f5cd3b5a8e7b210fd11ac6212f774f204))),UInt32Impl(0))

// this contains the information needed to analyze our input during finalization
val inputInfo = P2PKHInputInfo(outPoint, amount, privKey.publicKey)
// inputInfo: P2PKHInputInfo = P2PKHInputInfo(TransactionOutPoint(76aca317ebbd73347d8c95c9af482c7670cb8e7a9efe383a2909e7eb7dd98ea1:0),10000 sats,ECPublicKey(037ad941b7ac16fba77a8f76b111e6254f4645116ed60d742e7641c41541231208))

// this is how much we are going to pay as a fee to the network
// for this example, we are going to pay 1 satoshi per byte
val feeRate = SatoshisPerByte(1.satoshi)
// feeRate: SatoshisPerByte = SatoshisPerByte(1 sat)

val changePrivKey = ECPrivateKey.freshPrivateKey
// changePrivKey: ECPrivateKey = Masked(ECPrivateKeyImpl)
val changeSPK = P2PKHScriptPubKey(pubKey = changePrivKey.publicKey)
// changeSPK: P2PKHScriptPubKey = pkh(c3a4486c18cddd735615b246bc0071aee44da46c)

// We chose a finalizer that adds a change output to our tx based on a fee rate
val finalizer = StandardNonInteractiveFinalizer(
    Vector(inputInfo),
    feeRate,
    changeSPK)
// finalizer: StandardNonInteractiveFinalizer = StandardNonInteractiveFinalizer(Vector(P2PKHInputInfo(TransactionOutPoint(76aca317ebbd73347d8c95c9af482c7670cb8e7a9efe383a2909e7eb7dd98ea1:0),10000 sats,ECPublicKey(037ad941b7ac16fba77a8f76b111e6254f4645116ed60d742e7641c41541231208))),SatoshisPerByte(1 sat),pkh(c3a4486c18cddd735615b246bc0071aee44da46c))

// We can now finalize the tx builder result from earlier with this finalizer
val unsignedTxF: Future[Transaction] = finalizer.buildTx(builderResult)
// unsignedTxF: Future[Transaction] = Future(Success(BaseTransaction(Int32Impl(2),Vector(TransactionInputImpl(TransactionOutPoint(76aca317ebbd73347d8c95c9af482c7670cb8e7a9efe383a2909e7eb7dd98ea1:0),EmptyScriptSignature,UInt32Impl(0))),Vector(TransactionOutput(5000 sats,pkh(5223486f5cd3b5a8e7b210fd11ac6212f774f204)), TransactionOutput(4774 sats,pkh(c3a4486c18cddd735615b246bc0071aee44da46c))),UInt32Impl(0))))

// We now turn to signing the unsigned transaction
// this contains all the information we need to
// validly sign the UTXO above
val utxoInfo = ScriptSignatureParams(inputInfo = inputInfo,
                                     prevTransaction = creditingTx,
                                     signers = Vector(privKey),
                                     hashType =
                                         HashType.sigHashAll)
// utxoInfo: ScriptSignatureParams[P2PKHInputInfo] = ScriptSignatureParams(P2PKHInputInfo(TransactionOutPoint(76aca317ebbd73347d8c95c9af482c7670cb8e7a9efe383a2909e7eb7dd98ea1:0),10000 sats,ECPublicKey(037ad941b7ac16fba77a8f76b111e6254f4645116ed60d742e7641c41541231208)),BaseTransaction(Int32Impl(1),Vector(),Vector(TransactionOutput(10000 sats,pkh(6d52a3bf4776ae367dee519bda02a3f6b03fad7a))),UInt32Impl(0)),Vector(Masked(ECPrivateKeyImpl)),SIGHASH_ALL(Int32Impl(1)))

// all of the UTXO spending information, since we only have
// one input, this is just one element
val utxoInfos: Vector[ScriptSignatureParams[InputInfo]] = Vector(utxoInfo)
// utxoInfos: Vector[ScriptSignatureParams[InputInfo]] = Vector(ScriptSignatureParams(P2PKHInputInfo(TransactionOutPoint(76aca317ebbd73347d8c95c9af482c7670cb8e7a9efe383a2909e7eb7dd98ea1:0),10000 sats,ECPublicKey(037ad941b7ac16fba77a8f76b111e6254f4645116ed60d742e7641c41541231208)),BaseTransaction(Int32Impl(1),Vector(),Vector(TransactionOutput(10000 sats,pkh(6d52a3bf4776ae367dee519bda02a3f6b03fad7a))),UInt32Impl(0)),Vector(Masked(ECPrivateKeyImpl)),SIGHASH_ALL(Int32Impl(1))))

// Yay! Now we use the RawTxSigner object to sign the tx.
// The 'sign' method is going produce a validly signed transaction
// This is going to iterate through each of the UTXOs and use
// the corresponding ScriptSignatureParams to produce a validly
// signed input. This UTXO has:
//   1: one input
//   2: outputs (destination and change outputs)
//   3: a fee rate of 1 satoshi/byte
val signedTx: Transaction = {
  val signedTxF = unsignedTxF.flatMap { unsignedTx =>
    RawTxSigner.sign(
        utx = unsignedTx,
        utxoInfos = utxoInfos,
        expectedFeeRate = feeRate
    )
  }
  Await.result(signedTxF, 30.seconds)
}
// signedTx: Transaction = BaseTransaction(Int32Impl(2),Vector(TransactionInputImpl(TransactionOutPoint(76aca317ebbd73347d8c95c9af482c7670cb8e7a9efe383a2909e7eb7dd98ea1:0),P2PKHScriptSignature(ECPublicKey(037ad941b7ac16fba77a8f76b111e6254f4645116ed60d742e7641c41541231208), ECDigitalSignature(3045022100fe3fb06da1aed7204688a08ddea3be5ac33cfebd9dbeb57e09273b180c4ecfff02202be564c2a51fe23406da1c2da4cfbd53caaf53ac01b6b5a7cd6826435fc1911501)),UInt32Impl(0))),Vector(TransactionOutput(5000 sats,pkh(5223486f5cd3b5a8e7b210fd11ac6212f774f204)), TransactionOutput(4774 sats,pkh(c3a4486c18cddd735615b246bc0071aee44da46c))),UInt32Impl(0))
```

```scala
signedTx.inputs.length
// res2: Int = 1

signedTx.outputs.length
// res3: Int = 2

//remember, you can call .hex on any bitcoin-s data structure to get the hex representation!
signedTx.hex
// res4: String = 0200000001a18ed97debe709293a38fe9e7a8ecb70762c48afc9958c7d3473bdeb17a3ac76000000006b483045022100fe3fb06da1aed7204688a08ddea3be5ac33cfebd9dbeb57e09273b180c4ecfff02202be564c2a51fe23406da1c2da4cfbd53caaf53ac01b6b5a7cd6826435fc191150121037ad941b7ac16fba77a8f76b111e6254f4645116ed60d742e7641c41541231208000000000288130000000000001976a9145223486f5cd3b5a8e7b210fd11ac6212f774f20488aca6120000000000001976a914c3a4486c18cddd735615b246bc0071aee44da46c88ac00000000
```
