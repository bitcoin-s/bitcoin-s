package org.bitcoins.crypto

import org.bitcoins.crypto.MuSig2Util._

class MuSig2UtilTest extends BitcoinSCryptoTest {
  behavior of "MuSig2Util"

  it should "work for two parties" in {
    val priv1 = ECPrivateKey.freshPrivateKey
    val pub1 = priv1.publicKey
    val (noncePub1: MultiNoncePub, noncePriv1: MultiNoncePriv) = genMultiNonce()
    val priv2 = ECPrivateKey.freshPrivateKey
    val pub2 = priv2.publicKey
    val (noncePub2: MultiNoncePub, noncePriv2: MultiNoncePriv) = genMultiNonce()
    val keySet: KeySet = Vector(pub1, pub2)
    val msg = CryptoUtil.sha256(keySetSerialize(keySet)).bytes
    val aggMultiNoncePub = aggNonces(Vector(noncePub1, noncePub2))
    val (aggNonce1, s1) = sign(noncePriv1, aggMultiNoncePub, priv1, msg, keySet)
    val (aggNonce2, s2) = sign(noncePriv2, aggMultiNoncePub, priv2, msg, keySet)

    assert(aggNonce1 == aggNonce2)

    val sig = signAgg(Vector(s1, s2), aggNonce1)
    val aggPub = keyAgg(keySet)

    // This currently fails, need to debug, starting with
    // TODO check all of the ECPublicKey vs SchnorrPubKey vs SchnorrNonce stuff
    // TODO should be using the actual BIP340 signature hash instead of a custom one
    // TODO key ordering before aggregation
    // TODO partial signature verification
    // TODO MuSig2* optimization
    // TODO SessionContext and renaming of defs
    // TODO implement tweaking
    assert(verify(aggPub, msg, sig))
  }
}
