---
id: version-0.2.0-txbuilder
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
                                                HashType.sigHashAll,
                                               conditionalPath =
                                                ConditionalPath.NoConditionsLeft)

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
// res2: String = "0200000001aaa194af011ead436c86eddd507b9a02fc7b15b190d6975fa68757f5cf6ef640000000006a473044022070862947a8ee417f0873e9edb9484684c3671b109e1f69403135220bf4c9687c02207d2395edb2a88522d42fb3e34c6e86f8b81b333b18759dfb6a78ed978d30a07c01210342b79cc77b59e28a5f569348e9efd5fa9a37982f18c1d29b0818bc8890a1c60b000000000288130000000000001976a9146c9e69cc34268e73bbf02c0819d92db359385cd188aca6120000000000001976a914e5050227c7823cb7c5da0f8d879b1562b4ca581c88ac00000000"
```
