---
id: txbuilder
title: TxBuilder example
---

Bitcoin-S features a transaction builder that constructs and signs Bitcoin
transactions. Here's an example of how to use it

```scala mdoc:silent
import scala.concurrent._
import scala.concurrent.duration._

import org.bitcoins.core._
import number._
import config._
import currency._
import crypto._
import script.crypto._
import protocol.transaction._
import protocol.script._

import wallet.builder._
import wallet.fee._
import wallet.utxo._

implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

// generate a fresh private key that we are going to use in the scriptpubkey
val privKey = ECPrivateKey.freshPrivateKey
val pubKey = privKey.publicKey

// this is the script that the TxBuilder is going to create a
// script signature that validly spends this scriptPubKey
val creditingSpk = P2PKHScriptPubKey(pubKey = privKey.publicKey)
val amount = 10000.satoshis

// this is the UTXO we are going to be spending
val utxo =
  TransactionOutput(value = amount, scriptPubKey = creditingSpk)

// the private key that locks the funds for the script we are spending too
val destinationPrivKey = ECPrivateKey.freshPrivateKey

// the amount we are sending -- 5000 satoshis -- to the destinationSPK
val destinationAmount = 5000.satoshis

// the script that corresponds to destination private key, this is what is protecting the money
val destinationSPK =
  P2PKHScriptPubKey(pubKey = destinationPrivKey.publicKey)

// this is where we are sending money too
// we could add more destinations here if we
// wanted to batch transactions
val destinations = {
    val destination1 = TransactionOutput(value = destinationAmount,
                                         scriptPubKey = destinationSPK)

    Vector(destination1)
}

// we have to fabricate a transaction that contains the
// UTXO we are trying to spend. If this were a real blockchain
// we would need to reference the UTXO set
val creditingTx = BaseTransaction(version = Int32.one,
                                  inputs = Vector.empty,
                                  outputs = Vector(utxo),
                                  lockTime = UInt32.zero)

// this is the information we need from the crediting TX
// to properly "link" it in the transaction we are creating
val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

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

// all of the UTXO spending information, since we are only
//spending one UTXO, this is just one element
val utxos: Vector[BitcoinUTXOSpendingInfoFull] = Vector(utxoSpendingInfo)

// this is how much we are going to pay as a fee to the network
// for this example, we are going to pay 1 satoshi per byte
val feeRate = SatoshisPerByte(1.satoshi)

val changePrivKey = ECPrivateKey.freshPrivateKey
val changeSPK = P2PKHScriptPubKey(pubKey = changePrivKey.publicKey)

// the network we are on, for this example we are using
// the regression test network. This is a network you control
// on your own machine
val networkParams = RegTest

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
```

```scala mdoc
signedTx.inputs.length

signedTx.outputs.length

//remember, you can call .hex on any bitcoin-s data structure to get the hex representation!
signedTx.hex
```

Alternatively to the signing logic above, you can also sign with a single key
in the case of a multi-sig input. Here's an example of how to use it:

```scala mdoc
// generate a fresh private key that we are going to use in the scriptpubkey
val myKey = ECPrivateKey.freshPrivateKey
val myPubKey = privKey.publicKey

val otherPartyKey = ECPublicKey.freshPublicKey

// this is the script that the TxBuilder is going to create a
// script signature that validly spends this scriptPubKey
val creditingSpk = MultiSignatureScriptPubKey(2, Vector(myPubKey, otherPartyKey))
val amount = 10000.satoshis

// this is the UTXO we are going to be spending
val utxo =
  TransactionOutput(value = amount, scriptPubKey = creditingSpk)

// the private key that locks the funds for the script we are spending too
val destinationPrivKey = ECPrivateKey.freshPrivateKey

// the amount we are sending -- 5000 satoshis -- to the destinationSPK
val destinationAmount = 5000.satoshis

// the script that corresponds to destination private key, this is what is protecting the money
val destinationSPK =
  P2PKHScriptPubKey(pubKey = destinationPrivKey.publicKey)

// this is where we are sending money too
// we could add more destinations here if we
// wanted to batch transactions
val destinations = {
    val destination1 = TransactionOutput(value = destinationAmount,
                                         scriptPubKey = destinationSPK)

    Vector(destination1)
}

// we have to fabricate a transaction that contains the
// UTXO we are trying to spend. If this were a real blockchain
// we would need to reference the UTXO set
val creditingTx = BaseTransaction(version = Int32.one,
                                  inputs = Vector.empty,
                                  outputs = Vector(utxo),
                                  lockTime = UInt32.zero)

// this is the information we need from the crediting TX
// to properly "link" it in the transaction we are creating
val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

val inputs = {
    val input1 = TransactionInput(outPoint = outPoint,
                                  scriptSignature = EmptyScriptSignature,
                                  sequenceNumber = UInt32.zero)
    Vector(input1)
}

// this is the transaction we are constructing and signing
val spendingTx = BaseTransaction(version = Int32.one,
                                 inputs = inputs,
                                 outputs = destinations,
                                 lockTime = UInt32.zero)

// this contains all the information we need to
// validly sign the UTXO above with our key
val utxoSpendingInfo = BitcoinUTXOSpendingInfoSingle(outPoint = outPoint,
                                                   output = utxo,
                                                   signer = myKey,
                                                   redeemScriptOpt = Some(creditingSpk),
                                                   scriptWitnessOpt = None,
                                                   hashType =
                                                       HashType.sigHashAll,
                                                   conditionalPath =
                                                       ConditionalPath.NoConditionsLeft)

// Finally, we can use this spending info to sign with our key
// This will return a PartialSignature that contains the public key
// that the input was signed with as well as the signature
// this can be used in a PSBT or passed to another party in another form
val signedF = BitcoinSignerSingle.signSingle(spendingInfo = utxoSpendingInfo,
                                                   unsignedTx = spendingTx,
                                                   isDummySignature = false)
```