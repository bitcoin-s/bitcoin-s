package org.bitcoins.crypto

class SchnorrAdaptorTest extends BitcoinSCryptoTest {
  behavior of "SchnorrAdaptorSignature"

  it must "be able to symmetrically verify adaptor sigs, complete them and extract the secret" in {
    forAll(CryptoGenerators.nonZeroPrivKey,
           CryptoGenerators.nonZeroPrivKey,
           NumberGenerator.bytevector(32),
           NumberGenerator.bytevector(32)) {
      case (privateKey, adaptorSecret, dataToSign, auxRand) =>
        val adaptorPoint = adaptorSecret.publicKey
        val adaptorSig = AdaptorUtil.schnorrAdaptorSign(privateKey = privateKey,
                                                        adaptorPoint =
                                                          adaptorPoint,
                                                        dataToSign = dataToSign,
                                                        auxRand = auxRand)
        val verify =
          AdaptorUtil.schnorrAdaptorVerify(adaptorSig = adaptorSig,
                                           pubKey =
                                             privateKey.publicKey.toXOnly,
                                           data = dataToSign,
                                           adaptor = adaptorPoint)
        assert(verify, s"Failed to verify adaptor signature")

        val extractedAdaptor =
          AdaptorUtil.schnorrExtractAdaptor(dataToSign,
                                            privateKey.publicKey.toXOnly,
                                            adaptorSig)
        assert(extractedAdaptor == adaptorPoint,
               s"Failed to extract correct adaptor")

        val completedSig =
          AdaptorUtil.schnorrAdaptorComplete(adaptorSecret, adaptorSig)

        assert(privateKey.publicKey.schnorrPublicKey.verify(dataToSign,
                                                            completedSig),
               s"Failed to verify completed signature")

        val extractedAdaptorSecret =
          AdaptorUtil.schnorrExtractSecret(completedSig,
                                           adaptorSig,
                                           adaptorPoint)
        assert(extractedAdaptorSecret == adaptorSecret,
               s"Failed to extract correct adaptor secret")
    }
  }
}
