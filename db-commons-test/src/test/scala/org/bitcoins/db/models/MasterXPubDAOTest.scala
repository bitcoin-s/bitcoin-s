package org.bitcoins.db.models

import org.bitcoins.core.crypto.{ExtKeyVersion, ExtPrivateKey}
import org.bitcoins.testkit.db.{TestAppConfig, TestAppConfigFixture}

import java.sql.SQLException

class MasterXPubDAOTest extends TestAppConfigFixture {
  behavior of "MasterXPubDAO"

  it must "create and find a master xpub" in { testAppConfig: TestAppConfig =>
    val xpriv = ExtPrivateKey.freshRootKey(ExtKeyVersion.SegWitTestNet3Priv)
    val xpub = xpriv.extPublicKey
    val masterXpub =
      MasterXPubDAO()(executionContext, appConfig = testAppConfig)

    val createdF = masterXpub.create(xpub)

    val readF = createdF.flatMap { _ =>
      masterXpub.read(xpriv.publicKey)
    }

    for {
      readOpt <- readF
    } yield {
      readOpt match {
        case None    => fail()
        case Some(r) => assert(r.toExtPublicKey == xpub)
      }
    }
  }

  it must "create only one master xpub" in { testAppConfig: TestAppConfig =>
    val masterXpubDAO =
      MasterXPubDAO()(executionContext, appConfig = testAppConfig)

    val xpriv = ExtPrivateKey.freshRootKey(ExtKeyVersion.SegWitTestNet3Priv)
    val xpub = xpriv.extPublicKey

    val xpriv2 = ExtPrivateKey.freshRootKey(ExtKeyVersion.SegWitTestNet3Priv)
    val xpub2 = xpriv2.extPublicKey

    for {
      created <- masterXpubDAO.create(xpub)
      _ = assert(created.toExtPublicKey == xpub)
      // don't insert a duplicate key
      _ <- recoverToSucceededIf[SQLException](masterXpubDAO.create(xpub))
      // don't insert more than one key
      _ <- recoverToSucceededIf[SQLException](masterXpubDAO.create(xpub2))
    } yield succeed

  }

  it must "validate the masterxpub and succeed in the database" in {
    testAppConfig: TestAppConfig =>
      val xpriv = ExtPrivateKey.freshRootKey(ExtKeyVersion.SegWitTestNet3Priv)
      val xpub = xpriv.extPublicKey
      val masterXpub =
        MasterXPubDAO()(executionContext, appConfig = testAppConfig)

      val createdF = masterXpub.create(xpub)

      for {
        _ <- createdF
        _ <- masterXpub.validate(xpub)
      } yield succeed
  }

  it must "throw an exception is the stored master xpub is different than the given" in {
    testAppConfig: TestAppConfig =>
      val xpriv = ExtPrivateKey.freshRootKey(ExtKeyVersion.SegWitTestNet3Priv)
      val xpub = xpriv.extPublicKey
      val masterXpub =
        MasterXPubDAO()(executionContext, appConfig = testAppConfig)

      val createdF = masterXpub.create(xpub)

      val differentXpub = ExtPrivateKey
        .freshRootKey(ExtKeyVersion.SegWitTestNet3Priv)
        .extPublicKey

      val validatedF = {
        for {
          _ <- createdF
          _ <- masterXpub.validate(differentXpub)
        } yield ()
      }

      recoverToSucceededIf[RuntimeException] {
        validatedF
      }
  }
}
