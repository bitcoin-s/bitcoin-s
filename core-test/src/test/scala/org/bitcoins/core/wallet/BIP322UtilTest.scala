package org.bitcoins.core.wallet

import org.bitcoins.core.crypto.ECPrivateKeyUtil
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.crypto._
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits._

import scala.util.{Failure, Success}

class BIP322UtilTest extends BitcoinSUnitTest {

  val privKey: ECPrivateKey = ECPrivateKeyUtil
    .fromWIFToPrivateKey("cTsWy7DsaN2uKgVueRHcUKAP3mCNzX5r77GsiLWw9C9zapjiK8tQ")
    .toPrivateKey

  it must "create a correct toSpend and toSign transaction with a p2wpkh challenge" in {
    val message = "bip322-test-vector-1"

    val messageChallenge = P2WPKHWitnessSPKV0(privKey.publicKey)

    val BIP322Transactions(toSpend, toSign) =
      BIP322Util.createToSignTransaction(message, messageChallenge)

    val expectedToSpend = Transaction(
      hex"00000000010000000000000000000000000000000000000000000000000000000000000000ffffffff220020169ef330b8286ee1c004e0620bd24b260478f80f8090cbb0f5b59130f1de743600000000010000000000000000160014923dc99647d67edd123043f8f86366cb59abf32d00000000")

    val expectedToSign = Transaction(
      hex"000000000181c546bf535cbe9220a465258231cc88b093e60bf12d5cf86a99b05e0b202fbd000000000000000000010000000000000000016a00000000")

    assert(toSpend == expectedToSpend)
    assert(toSign == expectedToSign)
  }

  it must "correctly sign with a p2wpkh challenge" in {
    val message = "bip322-test-vector-1"
    val messageChallenge = P2WPKHWitnessSPKV0(privKey.publicKey)
    val bip322Txs =
      BIP322Util.createToSignTransaction(message, messageChallenge)

    val signedT =
      bip322Txs.psbt
        .sign(inputIndex = 0, signer = privKey)
        .finalizePSBT
        .flatMap(_.extractTransactionAndValidate)

    signedT match {
      case Failure(exception) => fail(exception)
      case Success(signed) =>
        val witness = P2WPKHWitnessV0(
          privKey.publicKey,
          ECDigitalSignature(
            "30440220281ab3b6f5ebb3e2f51af3ac43cba99712f3ea83c56156584235e6172debb2e7022058dcc448117467b2924e4d4d273de7f7b027f07b8e209609bac8db57ef9022c901")
        )
        val expected0 = WitnessTransaction
          .toWitnessTx(bip322Txs.toSign)
          .updateWitness(0, witness)

        val expected1 = Transaction(
          hex"0000000000010181c546bf535cbe9220a465258231cc88b093e60bf12d5cf86a99b05e0b202fbd000000000000000000010000000000000000016a024730440220281ab3b6f5ebb3e2f51af3ac43cba99712f3ea83c56156584235e6172debb2e7022058dcc448117467b2924e4d4d273de7f7b027f07b8e209609bac8db57ef9022c9012102fdbfbbfc35d0748ec58f628988090fce16985087e89639bccc4bfec4056323dc00000000")

        val base64 =
          "AAAAAAABAYHFRr9TXL6SIKRlJYIxzIiwk+YL8S1c+GqZsF4LIC+9AAAAAAAAAAAAAQAAAAAAAAAAAWoCRzBEAiAoGrO29euz4vUa86xDy6mXEvPqg8VhVlhCNeYXLeuy5wIgWNzESBF0Z7KSTk1NJz3n97An8HuOIJYJusjbV++QIskBIQL9v7v8NdB0jsWPYomICQ/OFphQh+iWObzMS/7EBWMj3AAAAAA="

        assert(signed == expected0)
        assert(signed == expected1)
        assert(signed.bytes.toBase64 == base64)
    }
  }

  it must "create a correct toSpend and toSign transaction with a p2pkh challenge" in {
    val message = "bip322-test-vector-2"
    val messageChallenge = P2PKHScriptPubKey(privKey.publicKey)

    val BIP322Transactions(toSpend, toSign) =
      BIP322Util.createToSignTransaction(message, messageChallenge)

    val expectedToSpend = Transaction(
      hex"00000000010000000000000000000000000000000000000000000000000000000000000000ffffffff220020ccc1ac760f75dff8927d5f3e9be84f31b4d37816e92903193def5383dcea4d06000000000100000000000000001976a914923dc99647d67edd123043f8f86366cb59abf32d88ac00000000")
    val expectedToSign = Transaction(
      hex"000000000121275fbf39c1a81b978e7ba2760d3a2bb211fb2da36ee8010b177d4e0e2dfe87000000000000000000010000000000000000016a00000000")

    assert(toSpend == expectedToSpend)
    assert(toSign == expectedToSign)
  }

  it must "correctly sign with a p2pkh challenge" in {
    val message = "bip322-test-vector-2"
    val messageChallenge = P2PKHScriptPubKey(privKey.publicKey)

    val bip322Txs =
      BIP322Util.createToSignTransaction(message, messageChallenge)
    val toSign = bip322Txs.toSign

    val signedT =
      bip322Txs.psbt
        .sign(inputIndex = 0, signer = privKey)
        .finalizePSBT
        .flatMap(_.extractTransactionAndValidate)

    signedT match {
      case Failure(exception) => fail(exception)
      case Success(signed) =>
        val scriptSig = P2PKHScriptSignature(
          ECDigitalSignature(
            "304402200562438a228539881d67f3f02e35ae8ad179f64a1db6bedbcd9ee9e59fac404b022005cc39beaef538f6cad8517ef6670aa0aa8d1c2f8c7f7aa5a3eb8a399cff5f9801"),
          privKey.publicKey
        )
        val oldInput = toSign.inputs.head
        val signedInput = TransactionInput(oldInput.previousOutput,
                                           scriptSig,
                                           oldInput.sequence)

        val expected0 = BaseTransaction(toSign.version,
                                        Vector(signedInput),
                                        toSign.outputs,
                                        toSign.lockTime)

        val expected1 = Transaction(
          hex"000000000121275fbf39c1a81b978e7ba2760d3a2bb211fb2da36ee8010b177d4e0e2dfe87000000006a47304402200562438a228539881d67f3f02e35ae8ad179f64a1db6bedbcd9ee9e59fac404b022005cc39beaef538f6cad8517ef6670aa0aa8d1c2f8c7f7aa5a3eb8a399cff5f98012102fdbfbbfc35d0748ec58f628988090fce16985087e89639bccc4bfec4056323dc00000000010000000000000000016a00000000")

        val base64 =
          "AAAAAAEhJ1+/OcGoG5eOe6J2DTorshH7LaNu6AELF31ODi3+hwAAAABqRzBEAiAFYkOKIoU5iB1n8/AuNa6K0Xn2Sh22vtvNnunln6xASwIgBcw5vq71OPbK2FF+9mcKoKqNHC+Mf3qlo+uKOZz/X5gBIQL9v7v8NdB0jsWPYomICQ/OFphQh+iWObzMS/7EBWMj3AAAAAABAAAAAAAAAAABagAAAAA="

        assert(signed == expected0)
        assert(signed == expected1)
        assert(signed.bytes.toBase64 == base64)
    }
  }

  it must "create a correct proof of funds tx" in {
    val outPoint0 = TransactionOutPoint(
      DoubleSha256DigestBE(
        "7fba3f6acc7b273abee9b86606c9aac7e4b123aa109b6f97f966e2268b054c9d"),
      UInt32.zero)
    val input0 = TransactionInput(outPoint0, EmptyScriptSignature, UInt32.zero)

    val outPoint1 = TransactionOutPoint(
      DoubleSha256DigestBE(
        "89ea67d5a08ebee86f163959c7f3101d4b7d903337fbfc4b1075a96d4db90736"),
      UInt32.zero)
    val input1 = TransactionInput(outPoint1, EmptyScriptSignature, UInt32.zero)

    val inputs = Vector(input0, input1)

    val BIP322Transactions(toSpend, toSign) =
      BIP322Util.createProofOfFundsTx(inputs)

    val expectedToSpend = Transaction(
      hex"00000000010000000000000000000000000000000000000000000000000000000000000000ffffffff220020c90c269c4f8fcbe6880f72a721ddfbf1914268a794cbb21cfafee13770ae19f100000000010000000000000000015100000000")
    val expectedToSign = Transaction(
      hex"0000000003ccdf46125ab9e3f5812bbb843d6f31cfe015eb8393648fcc4ef4bc702654bd910000000000000000009d4c058b26e266f9976f9b10aa23b1e4c7aac90666b8e9be3a277bcc6a3fba7f0000000000000000003607b94d6da975104bfcfb3733907d4b1d10f3c75939166fe8be8ea0d567ea89000000000000000000010000000000000000016a00000000")

    assert(toSpend == expectedToSpend)
    assert(toSign == expectedToSign)
  }

  it must "correctly sign a proof of funds tx" in {
    val outPoint0 = TransactionOutPoint(
      DoubleSha256DigestBE(
        "bd2f200b5eb0996af85c2df10be693b088cc31822565a42092be5c53bf46c581"),
      UInt32.zero)
    val input0 = TransactionInput(outPoint0, EmptyScriptSignature, UInt32.zero)

    val outPoint1 = TransactionOutPoint(
      DoubleSha256DigestBE(
        "87fe2d0e4e7d170b01e86ea32dfb11b22b3a0d76a27b8e971ba8c139bf5f2721"),
      UInt32.zero)
    val input1 = TransactionInput(outPoint1, EmptyScriptSignature, UInt32.zero)

    val inputs = Vector(input0, input1)

    val prevTx1 = Transaction(
      "00000000010000000000000000000000000000000000000000000000000000000000000000ffffffff220020169ef330b8286ee1c004e0620bd24b260478f80f8090cbb0f5b59130f1de743600000000010000000000000000160014923dc99647d67edd123043f8f86366cb59abf32d00000000")

    val prevTx2 = Transaction(
      "00000000010000000000000000000000000000000000000000000000000000000000000000ffffffff220020ccc1ac760f75dff8927d5f3e9be84f31b4d37816e92903193def5383dcea4d06000000000100000000000000001976a914923dc99647d67edd123043f8f86366cb59abf32d88ac00000000")

    val bip322Txs =
      BIP322Util.createProofOfFundsTx(inputs)

    val psbt = bip322Txs.psbt
      .addUTXOToInput(prevTx1, 1)
      .addUTXOToInput(prevTx2, 2)

    val signedT = psbt
      .sign(1, privKey)
      .sign(2, privKey)
      .finalizePSBT
      .flatMap(_.extractTransactionAndValidate)

    signedT match {
      case Failure(exception) => fail(exception)
      case Success(signed) =>
        val expected = Transaction(
          hex"00000000000103ccdf46125ab9e3f5812bbb843d6f31cfe015eb8393648fcc4ef4bc702654bd910000000001510000000081c546bf535cbe9220a465258231cc88b093e60bf12d5cf86a99b05e0b202fbd00000000000000000021275fbf39c1a81b978e7ba2760d3a2bb211fb2da36ee8010b177d4e0e2dfe87000000006a47304402203abf9cda10e3ddcd16b0e4a921dbdbc166d79186a17232177a5f4ec61e0aaa7502202f58993858e527493ae978f563499f4d724be71dc5b12833d294db2ce7fefb5d012102fdbfbbfc35d0748ec58f628988090fce16985087e89639bccc4bfec4056323dc00000000010000000000000000016a00024730440220367dfb3e5772b4d93c08d6fd06086d48487b154e78c8a3f132a07c34699f0f2102200b159f6d2ca88e777cbae5879a0f87b0fc03472f9a8a86865babd4436dde2d0f012102fdbfbbfc35d0748ec58f628988090fce16985087e89639bccc4bfec4056323dc0000000000")

        val base64 =
          "AAAAAAABA8zfRhJaueP1gSu7hD1vMc/gFeuDk2SPzE70vHAmVL2RAAAAAAFRAAAAAIHFRr9TXL6SIKRlJYIxzIiwk+YL8S1c+GqZsF4LIC+9AAAAAAAAAAAAISdfvznBqBuXjnuidg06K7IR+y2jbugBCxd9Tg4t/ocAAAAAakcwRAIgOr+c2hDj3c0WsOSpIdvbwWbXkYahcjIXel9Oxh4KqnUCIC9YmThY5SdJOul49WNJn01yS+cdxbEoM9KU2yzn/vtdASEC/b+7/DXQdI7Fj2KJiAkPzhaYUIfoljm8zEv+xAVjI9wAAAAAAQAAAAAAAAAAAWoAAkcwRAIgNn37PldytNk8CNb9BghtSEh7FU54yKPxMqB8NGmfDyECIAsVn20sqI53fLrlh5oPh7D8A0cvmoqGhlur1ENt3i0PASEC/b+7/DXQdI7Fj2KJiAkPzhaYUIfoljm8zEv+xAVjI9wAAAAAAA=="

        assert(signed == expected)
        assert(signed.bytes.toBase64 == base64)
    }
  }
}
