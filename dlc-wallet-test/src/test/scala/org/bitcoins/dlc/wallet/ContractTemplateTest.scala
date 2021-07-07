package org.bitcoins.dlc.wallet

import org.bitcoins.core.api.wallet.db.ContractTemplateDb
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import org.bitcoins.testkit.wallet._
import org.scalatest.FutureOutcome

class ContractTemplateTest extends BitcoinSWalletTest {

  override type FixtureParam = FundedDLCWallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withFundedDLCWallet(test, getBIP39PasswordOpt())(getFreshConfig)

  it must "create and retrieve a contract template" in { fundedWallet =>
    val wallet = fundedWallet.wallet

    val descriptor = DLCWalletUtil.sampleContractDescriptor.toTLV
    val expected = ContractTemplateDb("test label", descriptor, Satoshis.zero)

    for {
      created <- wallet.createContractTemplate(expected.label,
                                               expected.contractDescriptorTLV,
                                               expected.totalCollateral)
      all <- wallet.getContractTemplates
      found <- wallet.findContractTemplate(expected.label)
    } yield {
      assert(created == expected)
      assert(all == Vector(expected))
      assert(found.contains(expected))
    }
  }
}
