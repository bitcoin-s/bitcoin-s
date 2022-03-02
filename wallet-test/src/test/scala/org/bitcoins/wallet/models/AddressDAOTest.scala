package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.{
  AddressRecord,
  ScriptPubKeyDb,
  SegWitAddressDb
}
import org.bitcoins.core.hd.SegWitHDPath
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.script.{
  EmptyScriptWitness,
  P2WPKHWitnessSPKV0
}
import org.bitcoins.crypto.ECPublicKey
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

  it must "insert an address into the database whose script is already being watched" in {
    daos =>
      val spkDAO = daos.scriptPubKeyDAO
      val addressDAO = daos.addressDAO
      val addrStr = "bc1qfjex5a4m5w0atqrpwad3zj4vkfkuhun46tge9c"
      val address = Bech32Address.fromString(addrStr)
      val spk = address.scriptPubKey.asInstanceOf[P2WPKHWitnessSPKV0]
      //insert the script first
      val spkDb = ScriptPubKeyDb(address.scriptPubKey)
      val createdSpkF = spkDAO.create(spkDb)

      //now try to insert the address in the database
      val segwitHdPath: SegWitHDPath =
        SegWitHDPath.fromString("m/84'/0'/0'/0/0")
      val pubKey: ECPublicKey = ECPublicKey.freshPublicKey
      val addressDb = SegWitAddressDb.apply(segwitHdPath,
                                            pubKey,
                                            spk.pubKeyHash,
                                            address,
                                            EmptyScriptWitness,
                                            spk)
      for {
        createdSpk <- createdSpkF
        _ <- addressDAO.create(addressDb)
        //make sure we can find it now
        foundOpt <- addressDAO.read(address)
      } yield {
        assert(foundOpt.isDefined)
        assert(foundOpt.get.scriptPubKeyId == createdSpk.id.get)
        assert(foundOpt.get.address == address)
      }
  }
}
