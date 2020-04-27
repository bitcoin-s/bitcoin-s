package org.bitcoins.wallet.models

import java.sql.SQLException

import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}

class AddressDAOTest extends BitcoinSWalletTest with WalletDAOFixture {

  behavior of "AddressDAO"

  it should "fail to insert and read an address into the database without a corresponding account" in {
    daos =>
      val addressDAO = daos.addressDAO
      val readF = {
        val addressDb =
          WalletTestUtil.getAddressDb(WalletTestUtil.firstAccountDb)
        addressDAO.create(addressDb)
      }

      recoverToSucceededIf[SQLException](readF)
  }

  it should "insert and read an address into the database with a corresponding account" in {
    daos =>
      val accountDAO = daos.accountDAO
      val addressDAO = daos.addressDAO
      for {
        createdAccount <- {
          val account = WalletTestUtil.firstAccountDb
          accountDAO.create(account)
        }
        createdAddress <- {
          val addressDb = WalletTestUtil.getAddressDb(createdAccount)
          addressDAO.create(addressDb)
        }
        readAddress <- addressDAO.read(createdAddress.address)
      } yield assert(readAddress.contains(createdAddress))
  }
}
