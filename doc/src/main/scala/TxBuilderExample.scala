import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{Int32, Int64, UInt32}
import org.bitcoins.core.protocol.script.P2PKHScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfo
import org.scalatest.{FlatSpec, MustMatchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class TxBuilderExample extends FlatSpec with MustMatchers {

  behavior of "TxBuilderExample"

  it must "build a signed tx" in {

    //This is a documented example of how to create a signed bitcoin transaction
    //with bitcoin-s. You can run this test case with the following sbt command

    //$ sbt "doc/testOnly *TxBuilderExample -- -z signed"

    //generate a fresh private key that we are going to use in the scriptpubkey
    val privKey = ECPrivateKey.freshPrivateKey

    //this is the script that the TxBuilder is going to create a
    //script signature that validly spends this scriptPubKey
    val creditingSpk = P2PKHScriptPubKey(pubKey = privKey.publicKey)
    val amount = Satoshis(Int64(10000))

    //this is the utxo we are going to be spending
    val utxo =
      TransactionOutput(currencyUnit = amount, scriptPubKey = creditingSpk)

    //the private key that locks the funds for the script we are spending too
    val destinationPrivKey = ECPrivateKey.freshPrivateKey

    //the amount we are sending -- 5000 satoshis -- to the destinationSPK
    val destinationAmount = Satoshis(Int64(5000))

    //the script that corresponds to destination private key, this is what is protecting the money
    val destinationSPK =
      P2PKHScriptPubKey(pubKey = destinationPrivKey.publicKey)

    //this is where we are sending money too
    //we could add more destinations here if we
    //wanted to batch transactions
    val destinations = {
      val destination1 = TransactionOutput(currencyUnit = destinationAmount,
                                           scriptPubKey = destinationSPK)

      List(destination1)
    }

    //we have to fabricate a transaction that contains the
    //utxo we are trying to spend. If this were a real blockchain
    //we would need to reference the utxo set
    val creditingTx = BaseTransaction(version = Int32.one,
                                      inputs = List.empty,
                                      outputs = List(utxo),
                                      lockTime = UInt32.zero)

    //this is the information we need from the crediting tx
    //to properly "link" it in the transaction we are creating
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)

    // this contains all the information we need to
    // validly sign the utxo above
    val utxoSpendingInfo = BitcoinUTXOSpendingInfo(outPoint = outPoint,
                                                   output = utxo,
                                                   signers = List(privKey),
                                                   redeemScriptOpt = None,
                                                   scriptWitnessOpt = None,
                                                   hashType =
                                                     HashType.sigHashAll)

    //all of the utxo spending information, since we are only
    //spending one utxo, this is just one element
    val utxos: List[BitcoinUTXOSpendingInfo] = List(utxoSpendingInfo)

    //this is how much we are going to pay as a fee to the network
    //for this example, we are going to pay 1 satoshi per byte
    val feeRate = SatoshisPerByte(Satoshis.one)

    val changePrivKey = ECPrivateKey.freshPrivateKey
    val changeSPK = P2PKHScriptPubKey(pubKey = changePrivKey.publicKey)

    // the network we are on, for this example we are using
    // the regression test network. This is a network you control
    // on your own machine
    val networkParams = RegTest

    //yay! Now we have a TxBuilder object that we can use
    //to sign the tx.
    val txBuilder: Future[BitcoinTxBuilder] = {
      BitcoinTxBuilder(
        destinations = destinations,
        utxos = utxos,
        feeRate = feeRate,
        changeSPK = changeSPK,
        network = networkParams
      )
    }

    txBuilder.failed.foreach { case err => println(err.getMessage) }

    //let's finally produce a validly signed tx
    //The 'sign' method is going produce a validly signed transaction
    //This is going to iterate through each of the 'utxos' and use
    //the corresponding 'UTXOSpendingInfo' to produce a validly
    //signed input. This tx has a
    //
    //1 input
    //2 outputs (destination and change outputs)
    //3 a fee rate of 1 satoshi/byte
    val signedTxF: Future[Transaction] = txBuilder.flatMap(_.sign)

    //let's print these things out so you can example them
    signedTxF.map { tx =>
      println("\nInputs:")
      tx.inputs.foreach(println)

      println("\nOutputs:")
      tx.outputs.foreach(println)

      //here is the fully signed serialized tx that
      //you COULD broadcast to a cryptocurrency p2p network
      println(s"\nFully signed tx in hex:")

      println(s"${tx.hex}")
    }

    //The output from the print statements should read something like this

    //Inputs:
    //TransactionInputImpl(TransactionOutPointImpl(DoubleSha256DigestImpl(43c75d1d59e6f13f2ad3baf6e124685ba0919bccdbdf89c362fe2f30fee4bdfc),UInt32Impl(0)),P2PKHScriptSignature(6a4730440220573a7bbbd59192c4bf01b8f1dcafe981d11ab8528fead9d66d702c1b72e5dc76022007946a423073c949e85a4ca3901ab10a2d6b72873a347d2a55ef873016adae8601210356d581971934349333066ed933cdea45ae9c72829ce34d8dd6a758d56967e4cb),UInt32Impl(0))
    //
    //Outputs:
    //TransactionOutputImpl(SatoshisImpl(Int64Impl(5000)),P2PKHScriptPubKeyImpl(1976a914dbdadae42124c46a00d81181e5d9ab28fbf546ed88ac))
    //TransactionOutputImpl(SatoshisImpl(Int64Impl(4774)),P2PKHScriptPubKeyImpl(1976a914a95eb0d284593f0c8f818f64a55fa6e3852012a688ac))
    //
    //Fully signed tx in hex:
    //020000000143c75d1d59e6f13f2ad3baf6e124685ba0919bccdbdf89c362fe2f30fee4bdfc000000006a4730440220573a7bbbd59192c4bf01b8f1dcafe981d11ab8528fead9d66d702c1b72e5dc76022007946a423073c949e85a4ca3901ab10a2d6b72873a347d2a55ef873016adae8601210356d581971934349333066ed933cdea45ae9c72829ce34d8dd6a758d56967e4cb000000000288130000000000001976a914dbdadae42124c46a00d81181e5d9ab28fbf546ed88aca6120000000000001976a914a95eb0d284593f0c8f818f64a55fa6e3852012a688ac00000000

    //remember, you can call .hex on any bitcoin-s data structure to get the hex representation!
  }

}
