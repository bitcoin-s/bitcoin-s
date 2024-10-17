package org.bitcoins.commons.json

import org.bitcoins.commons.serializers.Picklers
import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.testkitcore.util.{BitcoinSUnitTest, TransactionTestUtil}

class SpendingInfoDbSerializerTest extends BitcoinSUnitTest {

  behavior of "SpendingInfoDbSerializer"

  it must "be symmetrical" in {
    val original = TransactionTestUtil.spendingInfoDb
    val json = upickle.default.writeJs[SpendingInfoDb](original)(
      using Picklers.spendingInfoDbPickler
    )

    val parsed =
      upickle.default.read(json)(using Picklers.spendingInfoDbPickler)

    assert(parsed == original)
  }
}
