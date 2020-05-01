package org.bitcoins.dlc

import org.bitcoins.core.protocol.script.{P2PKHScriptPubKey, P2WSHWitnessV0}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.{DoubleSha256DigestBE, ECPublicKey, Sha256DigestBE}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import scodec.bits.ByteVector

class SetupDLCTest extends BitcoinSAsyncTest {
  behavior of "SetupDLC"

  val dummyTxId: DoubleSha256DigestBE = DoubleSha256DigestBE.empty

  val dummyWitness: P2WSHWitnessV0 = P2WSHWitnessV0(
    P2PKHScriptPubKey(ECPublicKey.freshPublicKey))

  val validFundingTX: Transaction = Transaction(
    "0200000000010242314a67ffba50548dd03c67deed9dfacdbe47ab4b448ceb24ca4faa7a69863a0000000000ffffffffc65d01a3f18fd9ca3744ec874bb57291bd07b883feaf34434ac80f9b3733eab10000000000ffffffff03b82a0000000000002200200c3111a26bfc64a957c426dd70f7a8b09db535f9dee0728d461ee20230cd760c4288e11100000000160014a9c72ce64f26a0a74f44639868b28c5bb76e0e381290e1110000000016001436af2dd20039b196d33b16aa21441363f6a0f3ff0247304402207b5001391312630f7120c06b716b0a6cee3bc2bdb4d5e531b6860d6044cc0b5d02200bd895455e85bb51a8480cb1e987b884d17e66f12d86c4608bbbf1c73db407e60121032c45702df54ec7ed3082f2161bfe0074d4eb2653f4280146213c1f414d24e84402483045022100f4b904854e1c057198b4ae25de921488d2ef2abd00ac9f4dcb4d654b7e93760502202c555e6276c3bc502891a71ffa5b588ec19faf07d2252a2f2c6b4f43cc6f760201210340cc3d2d50246efc0747475c13d657db3bf36522e80fc13933fbd57fc02a628200000000")

  val validCET: Transaction = Transaction(
    "02000000000101f4bfdbbc2ce5c30c81752b8f9b9ed3af87ac372d4470e0d135086bd54e5983eb0000000000feffffff017e280000000000002200205c36d0bd00af0e2200cbb76f542dc21bd64128afd434400407982a7bc0b602fe0400473044022022d6184ff337b2235212d4219f96765cfc5112b7962ebdeaf8943b1902afb224022033b06495e91c19d3292f9ba7ff9c0682e12a407c0290c178f9d6c355a92ee8e6014730440220415edc452dcada9d42aa1aa61438b3b368cfae37937c41af7f67e1fe751c46a9022025253945b6ad9909dd73eca2282fd2111539cea11afbe51d14f127da080da3d701475221036e25bf2dfe81e426043f1a2c034387d72d360c4569cfe2aeec7d3afec270485c210308ce52011fb8e8d15c5a3d92acda4297097f7e08e3a1b3f178e0e25b3a2b6b8d52ae1f6d1900")

  val validCETInfo: CETInfo =
    CETInfo(validCET, dummyWitness, dummyTxId, dummyWitness)

  val validRefundTx: Transaction = Transaction(
    "02000000000101f4bfdbbc2ce5c30c81752b8f9b9ed3af87ac372d4470e0d135086bd54e5983eb0000000000feffffff02ce18000000000000160014dcbb4fc8023ef304aaa0dc6fec662567eba3575d43100000000000001600147770c55dff9cb4a4f23a0bbd0a75c29ea7fb8c310400483045022100becf83f0740f3f8c15327cc70b0b831e7f4d8a5dab0a9db5e2b6e4feb0ee02b4022059fa59b322c0cc5a3953fe22cbab1208859aa5d13cc286c752a48073ce18e5f0014830450221008449162719050c0e3c5c84a67468f8849ca395d3b422584a383923cd9ee51224022032a5319e6e863d07676f137e6d25c4bf9520b726e5c7274ce3e321199c5bd8b501475221036e25bf2dfe81e426043f1a2c034387d72d360c4569cfe2aeec7d3afec270485c210308ce52011fb8e8d15c5a3d92acda4297097f7e08e3a1b3f178e0e25b3a2b6b8d52ae216d1900")

  // These 2 can be the same, we only need them to have 1 input so we can do the correct checks
  val invalidCET: Transaction = Transaction(
    "02000000000101bdaa2ea7eea92a88f357bd8af92003286c5acb9e9b51006d2394b47afa1c7a040000000000ffffffff018a831e000000000017a914b80e1c53b48628277bd2cb63d9d111c8fbcecdda870400483045022100ee40ca5537b5a9e9aeb04e659a8e7ec8c2d4a33dd8f8e620c98561a9e87f2db802200c1a9e35464412b0c4c28125e0279807b1b0df649a10dbfce41fa52722d71ede01483045022100cea1701d3b7fc9ca7f83cfed3fbd43853d243249a0ece0ec7fbefe51d3c526df02202fed76ce12e54793961ef3611cefcd684c40ba4b640bb3a01abee7c1dd0fab6a01475221031454ce0a0354aadf6bd13f4e27c1287d33534dd02e249d4455341d528b7ea7192103ab051b06e850b33196ddd80b43084883a29c854c0c5a924b273cbe4b1c3a228452ae00000000")

  val invalidRefundTx: Transaction = invalidCET

  def setupDLC(
      fundingTx: Transaction = validFundingTX,
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
      setupDLC(cet0 = validCETInfo.copy(tx = validFundingTX)))
    assertThrows[IllegalArgumentException](
      setupDLC(cet1 = validCETInfo.copy(tx = validFundingTX)))
  }

  it must "not allow an invalid input for CETs" in {
    assertThrows[IllegalArgumentException](
      setupDLC(cet0 = validCETInfo.copy(tx = invalidCET)))
    assertThrows[IllegalArgumentException](
      setupDLC(cet1 = validCETInfo.copy(tx = invalidCET)))
  }

  it must "not allow an invalid number of inputs for the refundTx" in {
    // Funding tx has more than 1 input
    assertThrows[IllegalArgumentException](setupDLC(refundTx = validFundingTX))
  }

  it must "not allow an invalid input for the refundTx" in {
    assertThrows[IllegalArgumentException](setupDLC(refundTx = invalidRefundTx))
  }
}
