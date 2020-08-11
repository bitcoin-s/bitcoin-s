package org.bitcoins.wallet.models

import java.sql.SQLException

import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}

class AddressDAOTest extends BitcoinSWalletTest with WalletDAOFixture {

  behavior of "AddressDAO"

  it should "fail to insert and read an address into the database without a corresponding public key script" in {
    daos =>
      val addressDAO = daos.addressDAO
      val readF = {
        val addressDb =
          WalletTestUtil.getAddressDb(WalletTestUtil.firstAccountDb)
        addressDAO.create(AddressRecord.fromAddressDb(addressDb, -1))
      }
      recoverToSucceededIf[SQLException](readF)
  }

  it should "insert and read an address into the database with a corresponding public key script" in {
    daos =>
      val addressDAO = daos.addressDAO
      for {
        createdAddress <- {
          val addressDb =
            WalletTestUtil.getAddressDb(WalletTestUtil.firstAccountDb)
          addressDAO.create(addressDb)
        }
        readAddress <- {
          addressDAO.findAddress(createdAddress.address)
        }
      } yield assert(readAddress.contains(createdAddress))
  }
}
