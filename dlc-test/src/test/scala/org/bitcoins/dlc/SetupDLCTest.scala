package org.bitcoins.dlc

import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto._
import org.bitcoins.dlc.execution.{CETInfo, SetupDLC}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import scodec.bits.ByteVector

class SetupDLCTest extends BitcoinSAsyncTest {
  behavior of "SetupAdaptorDLC"

  val dummyTxId: DoubleSha256DigestBE = DoubleSha256DigestBE.empty

  val dummyAdaptorSig: ECAdaptorSignature = ECAdaptorSignature(
    ECPublicKey.freshPublicKey,
    ECPrivateKey.freshPrivateKey.fieldElement,
    ECPublicKey.freshPublicKey,
    ECPrivateKey.freshPrivateKey.fieldElement,
    ECPrivateKey.freshPrivateKey.fieldElement
  )

  val validFundingTx: Transaction = Transaction(
    "02000000000102f6ec63ebf8589895147803ea2724f1cc9fbcc75077136c15af57210ab8b4342b0000000000ffffffffbfeda85fa8d2335694b3602f0e0918fa7e9d144051217330c8a0328b4d0caabd0000000000ffffffff03c8c2eb0b0000000022002060b1a927da5c8570b1280d6dd0306d6458edc1c4abb7cc3ce8f75d157e78825e1ee0f50500000000160014558979e57695d211a354b87369ae819ae720e2761ee0f50500000000160014dc93c91963fb7b42860f92caf11217ef6c28599702483045022100b9189d799cdc4845b273ff4a471fcaa36e2419609bc3e749d78668bb8c2cb1350220403cdcddf10d06b8cb36e2943d1c8d195cbc0b023d1cdaa29281dfa250a744a9012103f5413f7d813a4e1bff925fa8b1e6ce154b309d6df4433cc1c6ded5e04f4d2d6202473044022001d25acb7ad50c352604a36d20fa25426d98d9d917200b017c052954ddf97e43022005b6d6c70d029206851c5af7ac3013fda87d5536003b0eee1c29cac6903cfbbd0121039ef319718ac70f4a3e86fb4eab23adc1c41dda9109f6c700b9b06690ddb3138b00000000")

  val validCET: Transaction = Transaction(
    "0200000000010112eb723473aa9ec2a91d82072b054feb4660389dc33ec407ff5836ff1ade73490000000000feffffff029014680300000000160014f161d1f494c1617a385f4480687b49826b5287ec70ad830800000000160014ea0f8ed8de8f6190bc67a2cf19f75bea97d0d582014752210258d139dc2f0507bd2b01794ff04530fd614a86dded69fd944da1942bcf748a7e2103eaf8df8e339381a19dfa5e37a4ce3c04ad3dc62f8d57774d92154e6d26ef06a452ae7c37265f"
  )

  val validCETInfo: CETInfo = CETInfo(validCET, dummyAdaptorSig)

  val validRefundTx: Transaction = Transaction(
    "0200000000010112eb723473aa9ec2a91d82072b054feb4660389dc33ec407ff5836ff1ade73490000000000feffffff02ace0f50500000000160014f161d1f494c1617a385f4480687b49826b5287ecabe0f50500000000160014ea0f8ed8de8f6190bc67a2cf19f75bea97d0d582040047304402206e204681682139ca91abca8a090c05d335c3077bcaa801d73ade4d30cf14befc0220114da42320f563f8df7ba29bfb26549b58b4dfc40d8af3a8e352b2da180fa9f001483045022100a9aa4a45d89d936762041cc2793700c3c6228326648a66228c4e265c9938337c0220024637e716c702176bde6029342ac42118a1af774ca6fb7a8225a31b15b1c839014752210258d139dc2f0507bd2b01794ff04530fd614a86dded69fd944da1942bcf748a7e2103eaf8df8e339381a19dfa5e37a4ce3c04ad3dc62f8d57774d92154e6d26ef06a452ae7d37265f")

  // These 2 can be the same, we only need them to have 1 input so we can do the correct checks
  val invalidCET: Transaction = Transaction(
    "02000000000101bdaa2ea7eea92a88f357bd8af92003286c5acb9e9b51006d2394b47afa1c7a040000000000ffffffff018a831e000000000017a914b80e1c53b48628277bd2cb63d9d111c8fbcecdda870400483045022100ee40ca5537b5a9e9aeb04e659a8e7ec8c2d4a33dd8f8e620c98561a9e87f2db802200c1a9e35464412b0c4c28125e0279807b1b0df649a10dbfce41fa52722d71ede01483045022100cea1701d3b7fc9ca7f83cfed3fbd43853d243249a0ece0ec7fbefe51d3c526df02202fed76ce12e54793961ef3611cefcd684c40ba4b640bb3a01abee7c1dd0fab6a01475221031454ce0a0354aadf6bd13f4e27c1287d33534dd02e249d4455341d528b7ea7192103ab051b06e850b33196ddd80b43084883a29c854c0c5a924b273cbe4b1c3a228452ae00000000"
  )

  val invalidRefundTx: Transaction = invalidCET

  def setupDLC(
      fundingTx: Transaction = validFundingTx,
      cet0: CETInfo = validCETInfo,
      cet1: CETInfo = validCETInfo,
      refundTx: Transaction = validRefundTx): SetupDLC = {
    SetupDLC(
      fundingTx = fundingTx,
      cets = Map(Sha256DigestBE(ByteVector.fill(32)(0.toByte)) -> cet0,
                 Sha256DigestBE(ByteVector.fill(32)(1.toByte)) -> cet1),
      refundTx = refundTx
    )
  }

  it must "not allow an invalid number of inputs for CETs" in {
    // Funding tx has more than 1 input
    assertThrows[IllegalArgumentException](
      setupDLC(cet0 = validCETInfo.copy(tx = validFundingTx)))
    assertThrows[IllegalArgumentException](
      setupDLC(cet1 = validCETInfo.copy(tx = validFundingTx)))
  }

  it must "not allow an invalid input for CETs" in {
    assertThrows[IllegalArgumentException](
      setupDLC(cet0 = validCETInfo.copy(tx = invalidCET)))
    assertThrows[IllegalArgumentException](
      setupDLC(cet1 = validCETInfo.copy(tx = invalidCET)))
  }

  it must "not allow an invalid number of inputs for the refundTx" in {
    // Funding tx has more than 1 input
    assertThrows[IllegalArgumentException](setupDLC(refundTx = validFundingTx))
  }

  it must "not allow an invalid input for the refundTx" in {
    assertThrows[IllegalArgumentException](setupDLC(refundTx = invalidRefundTx))
  }
}
