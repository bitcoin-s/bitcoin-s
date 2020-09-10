package org.bitcoins.wallet.models

import java.sql.SQLException

import org.bitcoins.core.api.wallet.db.AddressTagDb
import org.bitcoins.core.wallet.utxo.StorageLocationTag.HotStorage
import org.bitcoins.core.wallet.utxo.{
  AddressTag,
  UnknownAddressTag,
  UnknownAddressTagName,
  UnknownAddressTagType
}
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.util.TestUtil
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}
import org.scalatest.Assertion

import scala.concurrent.Future

class AddressTagDAOTest extends BitcoinSWalletTest with WalletDAOFixture {

  behavior of "AddressTagDAO"

  val exampleTag: UnknownAddressTag =
    UnknownAddressTag(UnknownAddressTagName("Example"),
                      UnknownAddressTagType("ExampleTagType"))

  def testInsertionFailure(
      daos: FixtureParam,
      tag: AddressTag): Future[Assertion] = {
    val tagDAO = daos.addressTagDAO
    val addr = TestUtil.testBitcoinAddress
    val tagDb = AddressTagDb(addr, tag.tagName, tag.tagType)
    val readF = tagDAO.create(tagDb)

    recoverToSucceededIf[SQLException](readF)
  }

  def testInsertion(daos: FixtureParam, tag: AddressTag): Future[Assertion] = {
    val accountDAO = daos.accountDAO
    val addressDAO = daos.addressDAO
    val addressTagDAO = daos.addressTagDAO
    for {
      createdAccount <- {
        val account = WalletTestUtil.firstAccountDb
        accountDAO.create(account)
      }
      createdAddress <- {
        val addressDb = WalletTestUtil.getAddressDb(createdAccount)
        addressDAO.create(addressDb)
      }
      createdAddressTag <- {
        val tagDb =
          AddressTagDb(createdAddress.address, tag)
        addressTagDAO.create(tagDb)
      }
      readAddressTagOpt <-
        addressTagDAO.findByAddress(createdAddressTag.address).map(_.headOption)
    } yield {
      assert(readAddressTagOpt.isDefined)
      val readAddressTag = readAddressTagOpt.get

      assert(readAddressTag.address == createdAddress.address)
      assert(readAddressTag.addressTag == tag)
    }
  }

  it should "fail to insert and read an unknown address tag into the database without a corresponding address" in {
    daos =>
      testInsertionFailure(daos, exampleTag)
  }

  it should "fail to insert and read an internal address tag into the database without a corresponding address" in {
    daos =>
      testInsertionFailure(daos, HotStorage)
  }

  it should "insert and read an unknown address tag into the database" in {
    daos =>
      testInsertion(daos, exampleTag)
  }

  it should "insert and read an internal address tag into the database" in {
    daos =>
      testInsertion(daos, HotStorage)
  }
}
