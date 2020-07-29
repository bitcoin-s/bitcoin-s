package org.bitcoins.dlc.testgen

import org.bitcoins.testkitcore.gen.CryptoGenerators
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class SchnorrSigPointTestVectorTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "SchnorrSigPointTestVector"

  it should "have serialization symmetry" in {
    val gen = for {
      privKey <- CryptoGenerators.privateKey
      privNonce <- CryptoGenerators.privateKey
      hash <- CryptoGenerators.sha256Digest
    } yield SchnorrSigPointTestVector(privKey, privNonce, hash)

    forAll(gen) { schnorrSigPointTest =>
      val schnorrSigPointTestResult =
        SchnorrSigPointTestVector.fromJson(schnorrSigPointTest.toJson)
      assert(schnorrSigPointTestResult.isSuccess)
      assert(schnorrSigPointTestResult.get == schnorrSigPointTest)
    }
  }

  it should "pass dlc_schnorr_test" in {
    val vecResult = SchnorrSigPointTestVectorGen.readFromDefaultTestFile()
    assert(vecResult.isSuccess)

    vecResult.get.foldLeft(succeed) { case (_, testVec) =>
      assert(SchnorrSigPointTestVector(testVec.inputs) == testVec)
      assert(
        testVec.pubKey.computeSigPoint(testVec.msgHash.bytes,
                                       testVec.pubNonce) == testVec.sigPoint)
    }
  }
}
