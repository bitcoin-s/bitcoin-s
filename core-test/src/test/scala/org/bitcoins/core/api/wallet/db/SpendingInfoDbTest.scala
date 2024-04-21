package org.bitcoins.core.api.wallet.db

import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkitcore.util.{BitcoinSUnitTest, TransactionTestUtil}

class SpendingInfoDbTest extends BitcoinSUnitTest {

  behavior of "SpendingInfoDbTest"

  it must "throw an exception if the spending txid is the same as txid" in {
    assertThrows[IllegalArgumentException] {
      TransactionTestUtil.spendingInfoDb.copy(spendingTxIdOpt =
        Some(DoubleSha256DigestBE.empty))
    }
  }
}
