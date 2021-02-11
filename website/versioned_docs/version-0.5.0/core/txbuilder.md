---
id: version-0.5.0-txbuilder
title: TxBuilder Example
original_id: txbuilder
---

Bitcoin-S features a transaction building API that allows you to construct and sign Bitcoin transactions. Here's an example of how to use it


```scala
implicit val ec: ExecutionContext = ExecutionContext.Implicits.global
// ec: ExecutionContext = scala.concurrent.impl.ExecutionContextImpl$$anon$3@3f5ad718[Running, parallelism = 16, size = 4, active = 0, running = 0, steals = 18, tasks = 0, submissions = 0]

// Initialize a transaction builder
val builder = RawTxBuilder()
// builder: RawTxBuilder = RawTxBuilder()

// generate a fresh private key that we are going to use in the scriptpubkey
val privKey = ECPrivateKey.freshPrivateKey
// privKey: ECPrivateKey = Masked(ECPrivateKeyImpl)
val pubKey = privKey.publicKey
// pubKey: ECPublicKey = ECPublicKey(0390a4b4751987bee83f38b4339919ec93fd83049c2b276958982357d82af9bc4a)

// this is the script that the TxBuilder is going to create a
// script signature that validly spends this scriptPubKey
val creditingSpk = P2PKHScriptPubKey(pubKey = privKey.publicKey)
// creditingSpk: P2PKHScriptPubKey = pkh(78f4a30c70decd2478a0d673f1ddc87c9b5abc44)
val amount = 10000.satoshis
// amount: Satoshis = 10000 sats

// this is the UTXO we are going to be spending
val utxo =
  TransactionOutput(value = amount, scriptPubKey = creditingSpk)
// utxo: TransactionOutput = TransactionOutput(10000 sats,pkh(78f4a30c70decd2478a0d673f1ddc87c9b5abc44))

// the private key that locks the funds for the script we are spending too
val destinationPrivKey = ECPrivateKey.freshPrivateKey
// destinationPrivKey: ECPrivateKey = Masked(ECPrivateKeyImpl)

// the amount we are sending -- 5000 satoshis -- to the destinationSPK
val destinationAmount = 5000.satoshis
// destinationAmount: Satoshis = 5000 sats

// the script that corresponds to destination private key, this is what is receiving the money
val destinationSPK =
  P2PKHScriptPubKey(pubKey = destinationPrivKey.publicKey)
// destinationSPK: P2PKHScriptPubKey = pkh(4606b8a9405b45223d6bfd3ce64f11f8616441fd)

// this is where we are sending money too
// we could add more destinations here if we
// wanted to batch transactions
val destinations = {
    val destination0 = TransactionOutput(value = destinationAmount,
                                         scriptPubKey = destinationSPK)

    Vector(destination0)
}
// destinations: Vector[TransactionOutput] = Vector(TransactionOutput(5000 sats,pkh(4606b8a9405b45223d6bfd3ce64f11f8616441fd)))

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
// creditingTx: BaseTransaction = BaseTransaction(Int32Impl(1),Vector(),Vector(TransactionOutput(10000 sats,pkh(78f4a30c70decd2478a0d673f1ddc87c9b5abc44))),UInt32Impl(0))

// this is the information we need from the crediting TX
// to properly "link" it in the transaction we are creating
val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
// outPoint: TransactionOutPoint = TransactionOutPoint(1370cec8820efad65001bfbd82b5e6efddbaab7e619dfa2865fd5cdf799be45f:0)
val input = TransactionInput(
    outPoint,
    EmptyScriptSignature,
    sequenceNumber = UInt32.zero)
// input: TransactionInput = TransactionInputImpl(TransactionOutPoint(1370cec8820efad65001bfbd82b5e6efddbaab7e619dfa2865fd5cdf799be45f:0),EmptyScriptSignature,UInt32Impl(0))

// Add a new input to our builder
builder += input
// res1: RawTxBuilder = RawTxBuilder()

// We can now generate a RawTxBuilderResult ready to be finalized
val builderResult = builder.result()
// builderResult: RawTxBuilderResult = RawTxBuilderResult(Int32Impl(2),Vector(TransactionInputImpl(TransactionOutPoint(1370cec8820efad65001bfbd82b5e6efddbaab7e619dfa2865fd5cdf799be45f:0),EmptyScriptSignature,UInt32Impl(0))),Vector(TransactionOutput(5000 sats,pkh(4606b8a9405b45223d6bfd3ce64f11f8616441fd))),UInt32Impl(0))

// this contains the information needed to analyze our input during finalization
val inputInfo = P2PKHInputInfo(outPoint, amount, privKey.publicKey)
// inputInfo: P2PKHInputInfo = P2PKHInputInfo(TransactionOutPoint(1370cec8820efad65001bfbd82b5e6efddbaab7e619dfa2865fd5cdf799be45f:0),10000 sats,ECPublicKey(0390a4b4751987bee83f38b4339919ec93fd83049c2b276958982357d82af9bc4a))

// this is how much we are going to pay as a fee to the network
// for this example, we are going to pay 1 satoshi per byte
val feeRate = SatoshisPerByte(1.satoshi)
// feeRate: SatoshisPerByte = 1 sats/byte

val changePrivKey = ECPrivateKey.freshPrivateKey
// changePrivKey: ECPrivateKey = Masked(ECPrivateKeyImpl)
val changeSPK = P2PKHScriptPubKey(pubKey = changePrivKey.publicKey)
// changeSPK: P2PKHScriptPubKey = pkh(1e2744c8005bbafe939e0ff625fd43fc385bcc6c)

// We chose a finalizer that adds a change output to our tx based on a fee rate
val finalizer = StandardNonInteractiveFinalizer(
    Vector(inputInfo),
    feeRate,
    changeSPK)
// finalizer: StandardNonInteractiveFinalizer = StandardNonInteractiveFinalizer(Vector(P2PKHInputInfo(TransactionOutPoint(1370cec8820efad65001bfbd82b5e6efddbaab7e619dfa2865fd5cdf799be45f:0),10000 sats,ECPublicKey(0390a4b4751987bee83f38b4339919ec93fd83049c2b276958982357d82af9bc4a))),1 sats/byte,pkh(1e2744c8005bbafe939e0ff625fd43fc385bcc6c))

// We can now finalize the tx builder result from earlier with this finalizer
val unsignedTxF: Future[Transaction] = finalizer.buildTx(builderResult)
// unsignedTxF: Future[Transaction] = Future(Success(BaseTransaction(Int32Impl(2),Vector(TransactionInputImpl(TransactionOutPoint(1370cec8820efad65001bfbd82b5e6efddbaab7e619dfa2865fd5cdf799be45f:0),EmptyScriptSignature,UInt32Impl(0))),Vector(TransactionOutput(5000 sats,pkh(4606b8a9405b45223d6bfd3ce64f11f8616441fd)), TransactionOutput(4775 sats,pkh(1e2744c8005bbafe939e0ff625fd43fc385bcc6c))),UInt32Impl(0))))

// We now turn to signing the unsigned transaction
// this contains all the information we need to
// validly sign the UTXO above
val utxoInfo = ScriptSignatureParams(inputInfo = inputInfo,
                                     prevTransaction = creditingTx,
                                     signers = Vector(privKey),
                                     hashType =
                                         HashType.sigHashAll)
// utxoInfo: ScriptSignatureParams[P2PKHInputInfo] = ScriptSignatureParams(P2PKHInputInfo(TransactionOutPoint(1370cec8820efad65001bfbd82b5e6efddbaab7e619dfa2865fd5cdf799be45f:0),10000 sats,ECPublicKey(0390a4b4751987bee83f38b4339919ec93fd83049c2b276958982357d82af9bc4a)),BaseTransaction(Int32Impl(1),Vector(),Vector(TransactionOutput(10000 sats,pkh(78f4a30c70decd2478a0d673f1ddc87c9b5abc44))),UInt32Impl(0)),Vector(Masked(ECPrivateKeyImpl)),SIGHASH_ALL(Int32Impl(1)))

// all of the UTXO spending information, since we only have
// one input, this is just one element
val utxoInfos: Vector[ScriptSignatureParams[InputInfo]] = Vector(utxoInfo)
// utxoInfos: Vector[ScriptSignatureParams[InputInfo]] = Vector(ScriptSignatureParams(P2PKHInputInfo(TransactionOutPoint(1370cec8820efad65001bfbd82b5e6efddbaab7e619dfa2865fd5cdf799be45f:0),10000 sats,ECPublicKey(0390a4b4751987bee83f38b4339919ec93fd83049c2b276958982357d82af9bc4a)),BaseTransaction(Int32Impl(1),Vector(),Vector(TransactionOutput(10000 sats,pkh(78f4a30c70decd2478a0d673f1ddc87c9b5abc44))),UInt32Impl(0)),Vector(Masked(ECPrivateKeyImpl)),SIGHASH_ALL(Int32Impl(1))))

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
// signedTx: Transaction = BaseTransaction(Int32Impl(2),Vector(TransactionInputImpl(TransactionOutPoint(1370cec8820efad65001bfbd82b5e6efddbaab7e619dfa2865fd5cdf799be45f:0),P2PKHScriptSignature(ECPublicKey(0390a4b4751987bee83f38b4339919ec93fd83049c2b276958982357d82af9bc4a), ECDigitalSignature(3044022033bcd24a8a264f29860594f27c03b1c8995d9b917b5d37e9a02561a83352483902204d3a39a8ab95ce3a82a664d0864a45f61da30bd59c7b102b97b76ed0b5c8086801)),UInt32Impl(0))),Vector(TransactionOutput(5000 sats,pkh(4606b8a9405b45223d6bfd3ce64f11f8616441fd)), TransactionOutput(4775 sats,pkh(1e2744c8005bbafe939e0ff625fd43fc385bcc6c))),UInt32Impl(0))
```

```scala
signedTx.inputs.length
// res2: Int = 1

signedTx.outputs.length
// res3: Int = 2

//remember, you can call .hex on any bitcoin-s data structure to get the hex representation!
signedTx.hex
// res4: String = 02000000015fe49b79df5cfd6528fa9d617eabbaddefe6b582bdbf0150d6fa0e82c8ce7013000000006a473044022033bcd24a8a264f29860594f27c03b1c8995d9b917b5d37e9a02561a83352483902204d3a39a8ab95ce3a82a664d0864a45f61da30bd59c7b102b97b76ed0b5c8086801210390a4b4751987bee83f38b4339919ec93fd83049c2b276958982357d82af9bc4a000000000288130000000000001976a9144606b8a9405b45223d6bfd3ce64f11f8616441fd88aca7120000000000001976a9141e2744c8005bbafe939e0ff625fd43fc385bcc6c88ac00000000
```
