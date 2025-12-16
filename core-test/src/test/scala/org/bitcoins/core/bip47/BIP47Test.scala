package org.bitcoins.core.bip47

import org.bitcoins.core.config.MainNet
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.ByteVector

class BIP47Test extends BitcoinSUnitTest {

  behavior of "BIP47"

  val testSeedAlice: ByteVector = ByteVector.fromValidHex(
    "64dca76abc9c6f0cf3d212d248c380c4622c8f93b2c425ec6a5567fd5db57e10d3e6f94a2f6af4ac2edb8998571f8664ec056d6281dc2fdc3fed1e8ae0eb7b5a"
  )

  val testSeedBob: ByteVector = ByteVector.fromValidHex(
    "87eaaac5a539ab028df44d9110f5b2a8ef4edce19cf8a7e4cffd5f4d826a7f53a0a1a10b2e4f8e35ad0c3f5e0c6d7c7e8d2a6f9b8c7d6e5f4a3b2c1d0e9f8a7b6"
  )

  it must "create a payment code from seed" in {
    val account = BIP47Account.fromSeed(testSeedAlice, MainNet, 0)
    val paymentCode = account.paymentCode

    assert(paymentCode.isValid)
    assert(paymentCode.pubKey.bytes.size == 33)
    assert(paymentCode.chainCode.bytes.size == 32)
  }

  it must "serialize and deserialize payment code symmetrically" in {
    val account = BIP47Account.fromSeed(testSeedAlice, MainNet, 0)
    val paymentCode = account.paymentCode

    val serialized = paymentCode.bytes
    val deserialized = PaymentCode.fromBytes(serialized)

    assert(deserialized.pubKey == paymentCode.pubKey)
    assert(deserialized.chainCode == paymentCode.chainCode)
    assert(deserialized.version == paymentCode.version)
  }

  it must "compute symmetric ECDH shared secrets" in {
    val aliceAccount = BIP47Account.fromSeed(testSeedAlice, MainNet, 0)
    val bobAccount = BIP47Account.fromSeed(testSeedBob, MainNet, 0)

    val alicePriv = aliceAccount.notificationKey.key
    val bobPriv = bobAccount.notificationKey.key

    val alicePub = alicePriv.publicKey
    val bobPub = bobPriv.publicKey

    val secretAliceToBob = SecretPoint(alicePriv, bobPub)
    val secretBobToAlice = SecretPoint(bobPriv, alicePub)

    assert(secretAliceToBob.ecdhSecret == secretBobToAlice.ecdhSecret)
  }

  it must "mask and unmask payment code payload" in {
    val account = BIP47Account.fromSeed(testSeedAlice, MainNet, 0)
    val payload = account.paymentCode.payload

    val fakeSecretPoint = ByteVector.fill(32)(0x42)
    val fakeOutpoint = ByteVector.fill(36)(0x01)

    val mask = PaymentCode.getMask(fakeSecretPoint, fakeOutpoint)
    val blinded = PaymentCode.blind(payload, mask)
    val unblinded = PaymentCode.blind(blinded, mask)

    assert(unblinded == payload)
  }

  it must "derive mutual payment addresses" in {
    val aliceAccount = BIP47Account.fromSeed(testSeedAlice, MainNet, 0)
    val bobAccount = BIP47Account.fromSeed(testSeedBob, MainNet, 0)

    val alicePaymentCode = aliceAccount.paymentCode
    val bobPaymentCode = bobAccount.paymentCode

    val aliceSendsToBob =
      aliceAccount.paymentAddressForSending(bobPaymentCode, 0)
    val bobReceivesFromAlice =
      bobAccount.paymentAddressForReceiving(alicePaymentCode, 0)

    val aliceSendAddress = aliceSendsToBob.segwitAddressForSend
    val bobReceiveAddress = bobReceivesFromAlice.segwitAddressForReceive

    assert(aliceSendAddress == bobReceiveAddress)
  }

  it must "generate valid segwit addresses" in {
    val account = BIP47Account.fromSeed(testSeedAlice, MainNet, 0)
    val address = account.addressAt(0)

    assert(address.value.startsWith("bc1"))
  }

  it must "derive keys at correct indices" in {
    val account = BIP47Account.fromSeed(testSeedAlice, MainNet, 0)

    val key0 = account.privateKeyAt(0)
    val key1 = account.privateKeyAt(1)

    assert(key0 != key1)
    assert(key0.publicKey != key1.publicKey)
  }

  it must "create watch-only account from payment code" in {
    val account = BIP47Account.fromSeed(testSeedAlice, MainNet, 0)
    val paymentCode = account.paymentCode

    val watchOnly = WatchOnlyBIP47Account(paymentCode, MainNet)

    assert(watchOnly.publicKeyAt(0) == account.publicKeyAt(0))
    assert(watchOnly.publicKeyAt(1) == account.publicKeyAt(1))
  }
}
