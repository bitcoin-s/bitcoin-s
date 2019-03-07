package org.bitcoins.core.crypto

import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

class SignTest extends BitcoinSUnitTest {
  implicit val ec = ExecutionContext.global

  //ECPrivateKey implements the sign interface
  //so just use it for testing purposes
  val signTestImpl = new Sign {
    private val key = ECPrivateKey.freshPrivateKey
    override def signFunction: ByteVector => Future[ECDigitalSignature] = {
      key.signFunction
    }

    override def publicKey: ECPublicKey = key.publicKey
  }

  behavior of "Sign"

  it must "sign arbitrary pieces of data correctly" in {
    forAll(CryptoGenerators.sha256Digest) {
      case hash: Sha256Digest =>
        val pubKey = signTestImpl.publicKey
        val sigF = signTestImpl.signFunction(hash.bytes)

        sigF.map(sig => assert(pubKey.verify(hash.hex, sig)))

    }
  }

}
