package org.bitcoins.dlc.wallet

import org.bitcoins.core.api.dlc.wallet.db.{DLCContactDb, DLCDb}
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.dlc.wallet.models.{DLCContactMapping, DLCContactMappingDb}
import org.bitcoins.testkit.fixtures.DLCDAOFixture
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, DLCWalletUtil}

import java.net.InetSocketAddress
import java.sql.SQLException

class DLCContactMappingDAOTest extends BitcoinSWalletTest with DLCDAOFixture {

  behavior of "DLCContactMappingDAO"

  val dlcDb1: DLCDb = DLCWalletUtil.sampleDLCDb

  val dlcDb2: DLCDb = DLCWalletUtil.sampleDLCDb.copy(
    dlcId = Sha256Digest.fromBytes(dlcDb1.dlcId.bytes.reverse),
    tempContractId =
      Sha256Digest.fromBytes(dlcDb1.tempContractId.bytes.reverse))

  val address1 = InetSocketAddress.createUnresolved("127.0.0.1", 1)
  val address2 = InetSocketAddress.createUnresolved("127.0.0.1", 2)

  val contactDb1: DLCContactDb = DLCContactDb(
    address = address1,
    alias = "1",
    memo = "memo 1"
  )

  val contactDb2: DLCContactDb = DLCContactDb(
    address = address2,
    alias = "2",
    memo = "memo 2"
  )

  it should "insert rows and enforce foreign keys" in { daos =>
    val dao = daos.dlcContactMappingDAO

    for {
      // no such DLC
      _ <- recoverToSucceededIf[SQLException](
        dao.create(
          DLCContactMappingDb(
            dlcDb1.dlcId,
            contactDb1.address
          )))

      _ <- daos.dlcDAO.create(dlcDb1)

      // no such contact
      _ <- recoverToSucceededIf[SQLException](
        dao.create(
          DLCContactMappingDb(
            dlcDb1.dlcId,
            contactDb1.address
          )))

      _ <- daos.contactDAO.create(contactDb1)

      // both DLC and contact exist
      created <- dao.create(
        DLCContactMappingDb(
          dlcDb1.dlcId,
          contactDb1.address
        ))
      read <- dao.read(dlcDb1.dlcId)
    } yield {
      assert(read.isDefined)
      assert(read.get == created)
      assert(created.dlcId == dlcDb1.dlcId)
      assert(created.contactId == contactDb1.address)
    }
  }

  it should "update rows and enforce foreign keys" in { daos =>
    val dao = daos.dlcContactMappingDAO

    for {
      _ <- daos.dlcDAO.create(dlcDb1)
      _ <- daos.contactDAO.create(contactDb1)
      _ <- dao.create(
        DLCContactMappingDb(
          dlcDb1.dlcId,
          contactDb1.address
        ))

      // no such contact
      _ <- recoverToSucceededIf[SQLException](
        dao.update(
          DLCContactMappingDb(
            dlcDb1.dlcId,
            contactDb2.address
          )))

      _ <- daos.contactDAO.create(contactDb2)

      updated <- dao.update(
        DLCContactMappingDb(
          dlcDb1.dlcId,
          contactDb2.address
        ))
      read <- dao.read(dlcDb1.dlcId)
    } yield {
      assert(read.isDefined)
      assert(read.get == updated)
      assert(updated.dlcId == dlcDb1.dlcId)
      assert(updated.contactId == contactDb2.address)
    }
  }

  it should "upsert rows and enforce foreign keys" in { daos =>
    val dao = daos.dlcContactMappingDAO

    for {
      // no such DLC
      _ <- recoverToSucceededIf[SQLException](
        dao.upsert(
          DLCContactMappingDb(
            dlcDb1.dlcId,
            contactDb1.address
          )))

      _ <- daos.dlcDAO.create(dlcDb1)

      // no such contact
      _ <- recoverToSucceededIf[SQLException](
        dao.upsert(
          DLCContactMappingDb(
            dlcDb1.dlcId,
            contactDb1.address
          )))

      _ <- daos.contactDAO.create(contactDb1)

      // both DLC and contact exist
      upserted1 <- dao.upsert(
        DLCContactMappingDb(
          dlcDb1.dlcId,
          contactDb1.address
        ))
      read1 <- dao.read(dlcDb1.dlcId)

      _ <- daos.contactDAO.create(contactDb2)

      upserted2 <- dao.upsert(
        DLCContactMappingDb(
          dlcDb1.dlcId,
          contactDb2.address
        ))
      read2 <- dao.read(dlcDb1.dlcId)
    } yield {
      assert(read1.isDefined)
      assert(read1.get == upserted1)
      assert(upserted1.dlcId == dlcDb1.dlcId)
      assert(upserted1.contactId == contactDb1.address)

      assert(read2.isDefined)
      assert(read2.get == upserted2)
      assert(upserted2.dlcId == dlcDb1.dlcId)
      assert(upserted2.contactId == contactDb2.address)
    }
  }

  it should "list all DLCs with and without associated contacts" in { daos =>
    for {
      _ <- daos.dlcDAO.create(dlcDb1)
      _ <- daos.dlcDAO.create(dlcDb2)
      _ <- daos.contactDAO.create(contactDb1)
      _ <- daos.dlcContactMappingDAO.create(dlcDb1, contactDb1)
      actual <- daos.dlcContactMappingDAO.listAll()
    } yield {
      assert(actual.size == 2)
      val expected = Vector(
        DLCContactMapping(dlcDb1, Some(contactDb1)),
        DLCContactMapping(dlcDb2, None)
      ).sortBy(_.dlc.dlcId.hex)
      assert(actual.sortBy(_.dlc.dlcId.hex) == expected)
    }
  }

  it should "find all DLCs by contact" in { daos =>
    for {
      _ <- daos.dlcDAO.create(dlcDb1)
      _ <- daos.dlcDAO.create(dlcDb2)
      _ <- daos.contactDAO.create(contactDb1)
      _ <- daos.dlcContactMappingDAO.create(dlcDb1, contactDb1)
      actual1 <- daos.dlcContactMappingDAO.findDLCsByContactId(contactId =
        contactDb1.address)
      actual2 <- daos.dlcContactMappingDAO.findDLCsByContactId(contactId =
        contactDb2.address)
    } yield {
      assert(actual1.size == 1)
      assert(actual2.isEmpty)
      val expected1 = Vector(dlcDb1)
      assert(actual1 == expected1)
    }
  }

  it should "find contact by DLCs" in { daos =>
    for {
      _ <- daos.dlcDAO.create(dlcDb1)
      _ <- daos.dlcDAO.create(dlcDb2)
      _ <- daos.contactDAO.create(contactDb1)
      _ <- daos.dlcContactMappingDAO.create(dlcDb1, contactDb1)
      actual1 <- daos.dlcContactMappingDAO.findContactByDLCId(dlcDb1.dlcId)
      actual2 <- daos.dlcContactMappingDAO.findContactByDLCId(dlcDb2.dlcId)
    } yield {
      assert(actual1.size == 1)
      assert(actual2.isEmpty)
      val expected1 = Some(contactDb1)
      assert(actual1 == expected1)
    }
  }

}
