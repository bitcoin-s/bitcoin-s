---
id: version-1.9.9-txbuilder
title: TxBuilder Example
original_id: txbuilder
---

Bitcoin-S features a transaction building API that allows you to construct and sign Bitcoin transactions. Here's an example of how to use it


```scala
implicit val ec: ExecutionContext = ExecutionContext.Implicits.global
// ec: ExecutionContext = scala.concurrent.impl.ExecutionContextImpl$$anon$3@7316ebb8[Running, parallelism = 8, size = 0, active = 0, running = 0, steals = 0, tasks = 0, submissions = 0]

// Initialize a transaction builder
val builder = RawTxBuilder()
// builder: RawTxBuilder = RawTxBuilder()

// generate a fresh private key that we are going to use in the scriptpubkey
val privKey = ECPrivateKey.freshPrivateKey
// privKey: ECPrivateKey = Masked(ECPrivateKey)
val pubKey = privKey.publicKey
// pubKey: ECPublicKey = ECPublicKey(02126ec203c2e1e7ad463d27486fbc955cf9cdecf402f685b4c044fffc80aedb5e)

// this is the script that the TxBuilder is going to create a
// script signature that validly spends this scriptPubKey
val creditingSpk = P2PKHScriptPubKey(pubKey = privKey.publicKey)
// creditingSpk: P2PKHScriptPubKey = pkh(29ae1a73d0a12613e624414f36c555ac327727a3)
val amount = 10000.satoshis
// amount: Satoshis = 10000 sats

// this is the UTXO we are going to be spending
val utxo =
  TransactionOutput(value = amount, scriptPubKey = creditingSpk)
// utxo: TransactionOutput = TransactionOutput(10000 sats,pkh(29ae1a73d0a12613e624414f36c555ac327727a3))

// the private key that locks the funds for the script we are spending too
val destinationPrivKey = ECPrivateKey.freshPrivateKey
// destinationPrivKey: ECPrivateKey = Masked(ECPrivateKey)

// the amount we are sending -- 5000 satoshis -- to the destinationSPK
val destinationAmount = 5000.satoshis
// destinationAmount: Satoshis = 5000 sats

// the script that corresponds to destination private key, this is what is receiving the money
val destinationSPK =
  P2PKHScriptPubKey(pubKey = destinationPrivKey.publicKey)
// destinationSPK: P2PKHScriptPubKey = pkh(082b7e53b2e65be14eee5fadcc28a279c7354922)

// this is where we are sending money too
// we could add more destinations here if we
// wanted to batch transactions
val destinations = {
    val destination0 = TransactionOutput(value = destinationAmount,
                                         scriptPubKey = destinationSPK)

    Vector(destination0)
}
// destinations: Vector[TransactionOutput] = Vector(TransactionOutput(5000 sats,pkh(082b7e53b2e65be14eee5fadcc28a279c7354922)))

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
// creditingTx: BaseTransaction = BaseTransaction(Int32Impl(1),Vector(),Vector(TransactionOutput(10000 sats,pkh(29ae1a73d0a12613e624414f36c555ac327727a3))),UInt32Impl(0))

// this is the information we need from the crediting TX
// to properly "link" it in the transaction we are creating
val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
// outPoint: TransactionOutPoint = TransactionOutPoint(453f318b92c49bcc9fffd2f685be192cd5d45e834c6cc7d76ae2d82a451f921a:0)
val input = TransactionInput(
    outPoint,
    EmptyScriptSignature,
    sequenceNumber = UInt32.zero)
// input: TransactionInput = TransactionInputImpl(TransactionOutPoint(453f318b92c49bcc9fffd2f685be192cd5d45e834c6cc7d76ae2d82a451f921a:0),EmptyScriptSignature,UInt32Impl(0))

// Add a new input to our builder
builder += input
// res1: RawTxBuilder = RawTxBuilder()

// We can now generate a RawTxBuilderResult ready to be finalized
val builderResult = builder.result()
// builderResult: RawTxBuilderResult = RawTxBuilderResult(Int32Impl(2),Vector(TransactionInputImpl(TransactionOutPoint(453f318b92c49bcc9fffd2f685be192cd5d45e834c6cc7d76ae2d82a451f921a:0),EmptyScriptSignature,UInt32Impl(0))),Vector(TransactionOutput(5000 sats,pkh(082b7e53b2e65be14eee5fadcc28a279c7354922))),UInt32Impl(0))

// this contains the information needed to analyze our input during finalization
val inputInfo = P2PKHInputInfo(outPoint, amount, privKey.publicKey)
// inputInfo: P2PKHInputInfo = P2PKHInputInfo(TransactionOutPoint(453f318b92c49bcc9fffd2f685be192cd5d45e834c6cc7d76ae2d82a451f921a:0),10000 sats,ECPublicKey(02126ec203c2e1e7ad463d27486fbc955cf9cdecf402f685b4c044fffc80aedb5e))

// this is how much we are going to pay as a fee to the network
// for this example, we are going to pay 1 satoshi per byte
val feeRate = SatoshisPerByte(1.satoshi)
// feeRate: SatoshisPerByte = 1 sats/byte

val changePrivKey = ECPrivateKey.freshPrivateKey
// changePrivKey: ECPrivateKey = Masked(ECPrivateKey)
val changeSPK = P2PKHScriptPubKey(pubKey = changePrivKey.publicKey)
// changeSPK: P2PKHScriptPubKey = pkh(74ddfa5250245ebff96e45a3c07744dc1c7206cc)

// We chose a finalizer that adds a change output to our tx based on a fee rate
val finalizer = StandardNonInteractiveFinalizer(
    Vector(inputInfo),
    feeRate,
    changeSPK)
// finalizer: StandardNonInteractiveFinalizer = StandardNonInteractiveFinalizer(Vector(P2PKHInputInfo(TransactionOutPoint(453f318b92c49bcc9fffd2f685be192cd5d45e834c6cc7d76ae2d82a451f921a:0),10000 sats,ECPublicKey(02126ec203c2e1e7ad463d27486fbc955cf9cdecf402f685b4c044fffc80aedb5e))),1 sats/byte,pkh(74ddfa5250245ebff96e45a3c07744dc1c7206cc))

// We can now finalize the tx builder result from earlier with this finalizer
val unsignedTx: Transaction = finalizer.buildTx(builderResult)
// unsignedTx: Transaction = BaseTransaction(Int32Impl(2),Vector(TransactionInputImpl(TransactionOutPoint(453f318b92c49bcc9fffd2f685be192cd5d45e834c6cc7d76ae2d82a451f921a:0),EmptyScriptSignature,UInt32Impl(0))),Vector(TransactionOutput(5000 sats,pkh(082b7e53b2e65be14eee5fadcc28a279c7354922)), TransactionOutput(4775 sats,pkh(74ddfa5250245ebff96e45a3c07744dc1c7206cc))),UInt32Impl(0))

// We now turn to signing the unsigned transaction
// this contains all the information we need to
// validly sign the UTXO above
val utxoInfo = ScriptSignatureParams(inputInfo = inputInfo,
                                     prevTransaction = creditingTx,
                                     signers = Vector(privKey),
                                     hashType =
                                         HashType.sigHashAll)
// utxoInfo: ScriptSignatureParams[P2PKHInputInfo] = ScriptSignatureParams(P2PKHInputInfo(TransactionOutPoint(453f318b92c49bcc9fffd2f685be192cd5d45e834c6cc7d76ae2d82a451f921a:0),10000 sats,ECPublicKey(02126ec203c2e1e7ad463d27486fbc955cf9cdecf402f685b4c044fffc80aedb5e)),BaseTransaction(Int32Impl(1),Vector(),Vector(TransactionOutput(10000 sats,pkh(29ae1a73d0a12613e624414f36c555ac327727a3))),UInt32Impl(0)),Vector(Masked(ECPrivateKey)),SIGHASH_ALL(1))

// all of the UTXO spending information, since we only have
// one input, this is just one element
val utxoInfos: Vector[ScriptSignatureParams[InputInfo]] = Vector(utxoInfo)
// utxoInfos: Vector[ScriptSignatureParams[InputInfo]] = Vector(ScriptSignatureParams(P2PKHInputInfo(TransactionOutPoint(453f318b92c49bcc9fffd2f685be192cd5d45e834c6cc7d76ae2d82a451f921a:0),10000 sats,ECPublicKey(02126ec203c2e1e7ad463d27486fbc955cf9cdecf402f685b4c044fffc80aedb5e)),BaseTransaction(Int32Impl(1),Vector(),Vector(TransactionOutput(10000 sats,pkh(29ae1a73d0a12613e624414f36c555ac327727a3))),UInt32Impl(0)),Vector(Masked(ECPrivateKey)),SIGHASH_ALL(1)))

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
// signedTx: Transaction = BaseTransaction(Int32Impl(2),Vector(TransactionInputImpl(TransactionOutPoint(453f318b92c49bcc9fffd2f685be192cd5d45e834c6cc7d76ae2d82a451f921a:0),P2PKHScriptSignature(ECPublicKeyBytes(ByteVector(33 bytes, 0x02126ec203c2e1e7ad463d27486fbc955cf9cdecf402f685b4c044fffc80aedb5e)), ECDigitalSignature(304402203d8f07fe2d56bb3bd27f152470eec0d5e9953c9c3a30470d426b29f6c920706202202b6feed57a744dfeca396842d0dad2f7e762a69d03ac02bd0e2e4e66247ce76d01)),UInt32Impl(0))),Vector(TransactionOutput(5000 sats,pkh(082b7e53b2e65be14eee5fadcc28a279c7354922)), TransactionOutput(4775 sats,pkh(74ddfa5250245ebff96e45a3c07744dc1c7206cc))),UInt32Impl(0))
```

```scala
signedTx.inputs.length
// res2: Int = 1

signedTx.outputs.length
// res3: Int = 2

//remember, you can call .hex on any bitcoin-s data structure to get the hex representation!
signedTx.hex
// res4: String = 02000000011a921f452ad8e26ad7c76c4c835ed4d52c19be85f6d2ff9fcc9bc4928b313f45000000006a47304402203d8f07fe2d56bb3bd27f152470eec0d5e9953c9c3a30470d426b29f6c920706202202b6feed57a744dfeca396842d0dad2f7e762a69d03ac02bd0e2e4e66247ce76d012102126ec203c2e1e7ad463d27486fbc955cf9cdecf402f685b4c044fffc80aedb5e000000000288130000000000001976a914082b7e53b2e65be14eee5fadcc28a279c735492288aca7120000000000001976a91474ddfa5250245ebff96e45a3c07744dc1c7206cc88ac00000000
```
