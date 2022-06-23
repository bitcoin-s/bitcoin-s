package org.bitcoins.crypto

import org.bitcoins.crypto.MuSig2Util._

class MuSig2UtilTest extends BitcoinSCryptoTest {
  behavior of "MuSig2Util"

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  it should "work for two parties" in {
    forAll(CryptoGenerators.privateKey,
           CryptoGenerators.privateKey,
           NumberGenerator.bytevector(32)) { case (priv1, priv2, msg) =>
      val pub1 = priv1.publicKey
      val (noncePub1: MultiNoncePub, noncePriv1: MultiNoncePriv) =
        genMultiNonce()
      val pub2 = priv2.publicKey
      val (noncePub2: MultiNoncePub, noncePriv2: MultiNoncePriv) =
        genMultiNonce()
      val keySet: KeySet = KeySet(pub1.schnorrPublicKey, pub2.schnorrPublicKey)
      val aggMultiNoncePub = aggNonces(Vector(noncePub1, noncePub2))
      val (aggNonce1, s1) =
        sign(noncePriv1, aggMultiNoncePub, priv1, msg, keySet)
      val (aggNonce2, s2) =
        sign(noncePriv2, aggMultiNoncePub, priv2, msg, keySet)

      assert(aggNonce1 == aggNonce2)

      val sig = signAgg(Vector(s1, s2), aggNonce1)
      val aggPub = keySet.aggPubKey

      assert(aggPub.schnorrPublicKey.verify(msg, sig))
    }
  }
}
