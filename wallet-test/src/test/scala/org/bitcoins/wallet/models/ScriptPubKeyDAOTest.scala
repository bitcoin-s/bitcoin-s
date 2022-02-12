package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.ScriptPubKeyDb
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.reserved._
import org.bitcoins.crypto.{DoubleSha256Digest, ECPublicKey}
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import scodec.bits.ByteVector

import java.sql.SQLException

class ScriptPubKeyDAOTest extends WalletDAOFixture {
  behavior of "ScriptPubKeyDAO"

  it must "be able to store and load spks" in { daos =>
    val scriptPubKeyDAO = daos.scriptPubKeyDAO

    val multisig = MultiSignatureScriptPubKey(
      2,
      Vector(ECPublicKey.freshPublicKey, ECPublicKey.freshPublicKey))

    val pkh = P2PKHScriptPubKey(ECPublicKey.freshPublicKey)

    val raw1 = RawScriptPubKey.fromAsmBytes(pkh.asmBytes)

    val raw2 = RawScriptPubKey.fromAsmBytes(multisig.asmBytes)

    val cltv = CLTVScriptPubKey(ScriptNumber.one, pkh)

    val spks = Vector(
      EmptyScriptPubKey,
      pkh,
      multisig,
      P2SHScriptPubKey(multisig),
      P2PKScriptPubKey(ECPublicKey.freshPublicKey),
      cltv,
      CSVScriptPubKey(ScriptNumber.one, pkh),
      NonStandardIfConditionalScriptPubKey(raw1, raw2),
      MultiSignatureWithTimeoutScriptPubKey(multisig, cltv),
      NonStandardNotIfConditionalScriptPubKey(raw1, raw2),
      P2PKWithTimeoutScriptPubKey(ECPublicKey.freshPublicKey,
                                  ScriptNumber.one,
                                  ECPublicKey.freshPublicKey),
      NonStandardScriptPubKey(Seq(OP_NOP)),
      P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey),
      P2WSHWitnessSPKV0(pkh),
      WitnessCommitment(
        DoubleSha256Digest(
          "0000000000000000000000000000000000000000000000000000000000000000"))
    ).map(spk => ScriptPubKeyDb(spk))

    for {
      _ <- scriptPubKeyDAO.createAll(spks)
      fromDb <- scriptPubKeyDAO.findAll()
    } yield {
      val actual = fromDb.sortBy(_.id)
      assert(actual.size == spks.size)
      assert(actual.map(_.scriptPubKey) == spks.map(_.scriptPubKey))
    }

  }

  it must "fail inserting non unique scripts" in { daos =>
    val scriptPubKeyDAO = daos.scriptPubKeyDAO
    val pkh = P2PKHScriptPubKey(ECPublicKey.freshPublicKey)
    val insertF = for {
      _ <- scriptPubKeyDAO.create(ScriptPubKeyDb(pkh))
      _ <- scriptPubKeyDAO.create(ScriptPubKeyDb(pkh))
    } yield {
      ()
    }
    recoverToSucceededIf[SQLException](insertF)
  }

  it must "be able to create new scripts if they don't exist" in { daos =>
    val scriptPubKeyDAO = daos.scriptPubKeyDAO
    val pkh = P2PKHScriptPubKey(ECPublicKey.freshPublicKey)
    for {
      db1 <- scriptPubKeyDAO.createIfNotExists(ScriptPubKeyDb(pkh))
      db2 <- scriptPubKeyDAO.createIfNotExists(ScriptPubKeyDb(pkh))
    } yield {
      assert(db1 == db2)
    }
  }

  it must "find a scriptPubKey" in { daos =>
    val scriptPubKeyDAO = daos.scriptPubKeyDAO
    val pkh = P2PKHScriptPubKey(ECPublicKey.freshPublicKey)
    val notInDb = P2PKHScriptPubKey(ECPublicKey.freshPublicKey)
    for {
      _ <- scriptPubKeyDAO.createIfNotExists(ScriptPubKeyDb(pkh))
      found <- scriptPubKeyDAO.findScriptPubKeys(Vector(pkh, notInDb))
    } yield {
      assert(found.length == 1)
      assert(found.head.scriptPubKey == pkh)
    }
  }

  it must "be able to store and find big spks" in { daos =>
    val scriptPubKeyDAO = daos.scriptPubKeyDAO

    val Size = 840011 // the size of the biggest script on testnet

    val big1 = RawScriptPubKey.fromAsmBytes(ByteVector.fill(Size)(0x55))
    assert(big1.bytes.size == Size + 5)

    val big2 = RawScriptPubKey.fromAsmBytes(ByteVector.fill(Size * 2)(0xaa))
    assert(big2.bytes.size == Size * 2 + 5)

    val multisig = MultiSignatureScriptPubKey(
      2,
      Vector(ECPublicKey.freshPublicKey, ECPublicKey.freshPublicKey))

    val pkh = P2PKHScriptPubKey(ECPublicKey.freshPublicKey)

    val spks = Vector(
      EmptyScriptPubKey,
      pkh,
      big1,
      multisig,
      big2
    ).map(spk => ScriptPubKeyDb(spk))

    for {
      _ <- scriptPubKeyDAO.createAll(spks)
      fromDb <- scriptPubKeyDAO.findAll()
      fromDb1 <- scriptPubKeyDAO.findScriptPubKeys(fromDb.map(_.scriptPubKey))
    } yield {
      assert(fromDb.sortBy(_.id) == fromDb1.sortBy(_.id))
      val actual = fromDb1.sortBy(_.id)
      assert(actual.size == spks.size)
      assert(actual.map(_.scriptPubKey) == spks.map(_.scriptPubKey))
    }
  }
}
