package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.AddressRecord
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.wallet.WalletTestUtil

import java.sql.SQLException

class AddressDAOTest extends WalletDAOFixture {

  behavior of "AddressDAO"

  it should "preserve public key scripts" in { daos =>
    val addressDAO = daos.addressDAO

    val addr1 = WalletTestUtil.getAddressDb(WalletTestUtil.firstAccountDb,
                                            addressIndex = 0)
    val addr2 = WalletTestUtil.getAddressDb(WalletTestUtil.firstAccountDb,
                                            addressIndex = 1)
    assert(addr1.scriptPubKey != addr2.scriptPubKey)

    for {
      created1 <- addressDAO.create(addr1)
      created2 <- addressDAO.create(addr2)
      found <- addressDAO.findAllAddresses()
    } yield {
      assert(addr1 == created1)
      assert(addr2 == created2)
      assert(
        Vector(addr1, addr2).sortBy(_.address.toString) == found.sortBy(
          _.address.toString))
    }
  }

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

  it should "find by script pub key" in { daos =>
    val addressDAO = daos.addressDAO

    val addr1 = WalletTestUtil.getAddressDb(WalletTestUtil.firstAccountDb)
    val addr2 = WalletTestUtil.getAddressDb(WalletTestUtil.firstAccountDb,
                                            addressIndex = 1)
    val addr3 = WalletTestUtil.getAddressDb(WalletTestUtil.firstAccountDb,
                                            addressIndex = 2)
    val spks = Vector(addr1.scriptPubKey, addr2.scriptPubKey)

    for {
      created1 <- addressDAO.create(addr1)
      created2 <- addressDAO.create(addr2)
      created3 <- addressDAO.create(addr3)
      found <- addressDAO.findByScriptPubKeys(spks)
    } yield {
      assert(found.contains(created1))
      assert(found.contains(created2))
      assert(!found.contains(created3))
    }
  }
}
