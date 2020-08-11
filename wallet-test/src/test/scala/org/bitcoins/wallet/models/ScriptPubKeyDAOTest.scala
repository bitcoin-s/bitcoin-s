package org.bitcoins.wallet.models

import java.sql.SQLException

import org.bitcoins.core.protocol.script.{
  CLTVScriptPubKey,
  CSVScriptPubKey,
  EmptyScriptPubKey,
  MultiSignatureScriptPubKey,
  MultiSignatureWithTimeoutScriptPubKey,
  NonStandardIfConditionalScriptPubKey,
  NonStandardNotIfConditionalScriptPubKey,
  NonStandardScriptPubKey,
  P2PKHScriptPubKey,
  P2PKScriptPubKey,
  P2PKWithTimeoutScriptPubKey,
  P2SHScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WSHWitnessSPKV0,
  RawScriptPubKey,
  WitnessCommitment
}
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.crypto.{DoubleSha256Digest, ECPublicKey}
import org.bitcoins.testkit.fixtures.{WalletDAOFixture}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.core.script.reserved._

class ScriptPubKeyDAOTest extends BitcoinSWalletTest with WalletDAOFixture {
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

}
