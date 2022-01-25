package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.protocol.dlc.compute.DLCUtil
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class DLCUtilTest extends BitcoinSUnitTest {

  behavior of "DLCUtil"

  it must "correctly compute the contractId" in {
    val txHex =
      "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff020000ffffffff0101000000000000000000000000"
    val tx = Transaction.fromHex(txHex)
    val outputIdx = 0
    val tempContractId = Sha256Digest.empty //32 bytes of 0x0000..000
    val contractId = DLCUtil.computeContractId(fundingTx = tx,
                                               outputIdx = outputIdx,
                                               tempContractId = tempContractId)

    val expected =
      "a3f942fe9cd3280f2e9b0e95a6228b4cc48b94f85a7f20129f89d3c3a80f4dd8"
    assert(contractId.toHex == expected)
  }

}
