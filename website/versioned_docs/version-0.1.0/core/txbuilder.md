---
id: version-0.1.0-txbuilder
title: TxBuilder example
original_id: txbuilder
---

Bitcoin-S features a transaction buidlder that constructs and signs Bitcoin
transactions. Here's an example of how to use it

```scala
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
  TransactionOutput(currencyUnit = amount, scriptPubKey = creditingSpk)

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
    val destination1 = TransactionOutput(currencyUnit = destinationAmount,
                                         scriptPubKey = destinationSPK)

    List(destination1)
}

// we have to fabricate a transaction that contains the
// UTXO we are trying to spend. If this were a real blockchain
// we would need to reference the UTXO set
val creditingTx = BaseTransaction(version = Int32.one,
                                  inputs = List.empty,
                                  outputs = List(utxo),
                                  lockTime = UInt32.zero)

// this is the information we need from the crediting TX
// to properly "link" it in the transaction we are creating
val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

// this contains all the information we need to
// validly sign the UTXO above
val utxoSpendingInfo = BitcoinUTXOSpendingInfo(outPoint = outPoint,
                                               output = utxo,
                                               signers = List(privKey),
                                               redeemScriptOpt = None,
                                               scriptWitnessOpt = None,
                                               hashType =
                                                HashType.sigHashAll)

// all of the UTXO spending information, since we are only
//spending one UTXO, this is just one element
val utxos: List[BitcoinUTXOSpendingInfo] = List(utxoSpendingInfo)

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

```scala
signedTx.inputs.length
// res0: Int = 1

signedTx.outputs.length
// res1: Int = 2

//remember, you can call .hex on any bitcoin-s data structure to get the hex representation!
signedTx.hex
// res2: String = "02000000011246e1e161b239715655cd320f01dbf182614c85a788bb61c8d393b740c355da000000006a47304402202221bf73ec3ee6d0e10b1804eb6926313a1ca927f266bb46122780df519881c502207d121507813ec4e6c5ee9c6d782252829e05e77418a41ae3c7aa74f45e41426401210282d386db7421d36dcd8a130725429789bf7f817a595a99aaad502e9e49c8d281000000000288130000000000001976a9145093de131f5ca169aed2c3f2c85eb77074920bea88aca6120000000000001976a9149e2f2ba93f21679d7676aa643489d7eca2362cb588ac00000000"
```

