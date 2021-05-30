package org.bitcoins.core.dlc.accounting

import org.bitcoins.core.config.MainNet
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.crypto.{ECPublicKey, Sha256Digest}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class DlcAccountingTest extends BitcoinSUnitTest {
  behavior of "DlcAccounting"

  private def getAddress(): BitcoinAddress = {
    Bech32Address.fromScriptPubKey(
      P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey),
      MainNet)
  }

  private val myPayoutAddress = getAddress()
  private val theirPayoutAddress = getAddress()

  it must "calculate basic pnl where we win all funds" in {
    val myCollateral = Satoshis(50000)
    val theirCollateral = myCollateral //symmetrical collateral

    val accounting1 = DlcAccounting(
      dlcId = Sha256Digest.empty,
      myCollateral = myCollateral,
      theirCollateral = theirCollateral,
      myPayoutAddress = myPayoutAddress,
      theirPayoutAddress = theirPayoutAddress,
      myPayout = myCollateral + theirCollateral,
      theirPayout = Satoshis.zero
    )

    //we make 50,000 sats (their collateral) is the profit
    assert(accounting1.pnl == theirCollateral)
    assert(accounting1.rorPrettyPrint == "100%")
  }

  it must "calculate basic pnl where we lose all funds" in {
    val myCollateral = Satoshis(50000)
    val theirCollateral = myCollateral //symmetrical collateral

    val accounting1 = DlcAccounting(
      dlcId = Sha256Digest.empty,
      myCollateral = Satoshis(50000),
      theirCollateral = Satoshis(50000),
      myPayoutAddress = myPayoutAddress,
      theirPayoutAddress = theirPayoutAddress,
      myPayout = Satoshis.zero,
      theirPayout = myCollateral + theirCollateral
    )

    //we lose 50,000 sats (my collateral) is the loss
    assert(accounting1.pnl == Satoshis(-50000))
    assert(accounting1.rateOfReturn == -1)
    assert(accounting1.rorPrettyPrint == "-100%")
  }

  it must "calculate basic pnl where funds are refunded" in {
    val myCollateral = Satoshis(50000)
    val theirCollateral = myCollateral //symmetrical collateral

    val accounting1 = DlcAccounting(
      dlcId = Sha256Digest.empty,
      myCollateral = Satoshis(50000),
      theirCollateral = Satoshis(50000),
      myPayoutAddress = myPayoutAddress,
      theirPayoutAddress = theirPayoutAddress,
      myPayout = myCollateral,
      theirPayout = theirCollateral
    )

    //collateral refunded, so no pnl
    assert(accounting1.pnl == Satoshis.zero)
    assert(accounting1.rateOfReturn == 0)
    assert(accounting1.rorPrettyPrint == "0%")
  }
}
