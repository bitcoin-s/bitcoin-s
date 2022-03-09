package org.bitcoins.wallet

import org.bitcoins.core.wallet.utxo.UnknownAddressTag
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.scalatest.FutureOutcome

import java.sql.SQLException

class AddressLabelTest extends BitcoinSWalletTest {
  type FixtureParam = FundedWallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withFundedWallet(test, getBIP39PasswordOpt())(getFreshWalletAppConfig)
  }

  behavior of "Address tags"

  it must "add two tags to the database" in { fundedWallet =>
    val wallet = fundedWallet.wallet
    val tag1 = UnknownAddressTag("test_tag_name_1", "test_tag_type_1")
    val tag2 = UnknownAddressTag("test_tag_name_2", "test_tag_type_2")
    val addressF = for {
      address <- wallet.getNewAddress(Vector(tag1))
      //add another tag to address
      tagDb1 <- wallet.getAddressTags(address)
      tagDb2 <- wallet.tagAddress(address, tag2)
    } yield {
      assert(tagDb1.head.address == address)
      assert(tagDb1.head.tagName == tag1.tagName)

      assert(tagDb2.tagName == tag2.tagName)
      assert(tagDb2.address == address)
    }

    addressF
  }

  it must "fail if we tag the address with the same tag twice" in {
    fundedWallet =>
      val wallet = fundedWallet.wallet
      val tag1 = UnknownAddressTag(tagName = "test_tag_name_1",
                                   tagType = "test_tag_type_1")
      val tag2 = UnknownAddressTag(tagName = "test_tag_name_1",
                                   tagType = "test_tag_type_2")
      val resultF = for {
        address <- wallet.getNewAddress()
        //add another tag to address
        _ <- wallet.tagAddress(address, tag1)
        _ <- wallet.tagAddress(address, tag2)
      } yield ()

      recoverToSucceededIf[SQLException](resultF)
  }
}
