package org.bitcoins.crypto.musig

import org.bitcoins.commons.serializers.JsonReaders.ECPublicKeyReads
import org.bitcoins.crypto.*
import play.api.libs.json.*

import scala.io.Source
import scala.util.Using

class Musig2TestVectors extends BitcoinSCryptoTest {
  behavior of "Musig2"

  it must "pass key_sort_vectors.json" in {

    val fileName = "/musig2/key_sort_vectors.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)

    val pubkeys: Vector[ECPublicKey] =
      (json \ "pubkeys").validate[Vector[ECPublicKey]].get

    val sortedPubkeys: Vector[ECPublicKey] =
      (json \ "sorted_pubkeys").validate[Vector[ECPublicKey]].get

    // Build UnsortedKeySet from parsed pubkeys and assert the sorted keys equal expected
    val keySet = UnsortedKeySet(pubkeys)
    val actualSorted = keySet.toSorted

    assert(actualSorted.keys == sortedPubkeys)
  }

  it must "pass key_agg_vectors.json" in {

    val fileName = "/musig2/key_agg_vectors.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)

    val parsed = json.validate[Musig2Json.KeyAggVectors]
    parsed.fold(
      errs => fail(s"Failed to parse key_agg_vectors.json: $errs"),
      vecs => {
        // Check the valid test cases
        vecs.valid_test_cases.foreach { tc =>
          val keys = tc.key_indices.map(idx => vecs.pubkeys(idx)).toVector
          val keySet = UnsortedKeySet(keys.map(_.toPublicKey))
          assert(keySet.aggPubKey.schnorrPublicKey == tc.expected)
        }

        // For error test cases we just ensure appropriate failures are raised
        vecs.error_test_cases.foreach { etc =>
          val keys = etc.key_indices.map(idx => vecs.pubkeys(idx)).toVector
          val tweaks = etc.tweak_indices.map(i => vecs.tweaks(i)).toVector
          // Build KeySet and expect either construction or aggPubKey to throw
          intercept[Exception] {
            val tweaksWXonly = tweaks.zip(etc.is_xonly)
            val musigTweaks = tweaksWXonly.map(t =>
              MuSigTweak(FieldElement.fromBytes(t._1), isXOnlyT = t._2))
            val kset =
              UnsortedKeySet(keys.map(_.toPublicKey))
                .withTweaks(musigTweaks)
            // Force aggPubKey computation
            kset.aggPubKey
          }
        }
      }
    )
  }

  it must "pass nonce_gen_vectors.json" in {
    val fileName = "/musig2/nonce_gen_vectors.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)

    val vecs = json.validate[Musig2Json.NonceGenVectors].get

    vecs.test_cases.foreach { test =>
      val preRand = test.rand_
      val noncePriv =
        MuSigUtil.nonceGen(
          preRand = preRand,
          publicKey = test.pk,
          privKeyOpt = test.sk.map(s => ECPrivateKey.fromBytes(s.bytes)),
          aggPubKeyOpt = test.aggpk,
          msgOpt = test.msg,
          extraInOpt = test.extra_in
        )
      val pubnonce = noncePriv.toNoncePub

      assert(
        noncePriv == test.expected_secnonce,
        s"expected=${test.expected_secnonce.toStringSensitive} actual=${noncePriv.toStringSensitive}")
      assert(pubnonce == test.expected_pubnonce)
    }
  }

  it must "pass nonce_agg_vectors.json" in {
    val fileName = "/musig2/nonce_agg_vectors.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)

    val vecs = json.validate[Musig2Json.NonceAggVectors].get

    // Check valid cases
    vecs.valid_test_cases.foreach { tc =>
      val pnonces =
        tc.pnonce_indices.map(i => MuSigNoncePub.fromBytes(vecs.pnonces(i)))
      val agg = MuSigNoncePub.aggregate(pnonces)
      assert(agg == tc.expected)
    }

    // Check error cases
    vecs.error_test_cases.foreach { etc =>
      intercept[Exception] {
        val pnonces =
          etc.pnonce_indices.map(i => MuSigNoncePub.fromBytes(vecs.pnonces(i)))
        MuSigNoncePub.aggregate(pnonces)
      }
    }
  }

  it must "pass sign_verify_vectors.json" in {
    val fileName = "/musig2/sign_verify_vectors.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)

    val vecs = json.validate[Musig2Json.SignVerifyVectors].get

    val signerPriv = ECPrivateKey.fromBytes(vecs.sk)

    // Helper to get MuSigNoncePriv default secnonce
    def defaultSecNonce: MuSigNoncePriv =
      MuSigNoncePriv.fromBytes(vecs.secnonces.head)

    // VALID sign tests
    vecs.valid_test_cases.foreach { tc =>
      val keys = tc.key_indices.map(i => vecs.pubkeys(i).toPublicKey).toVector
      val keySet = UnsortedKeySet(keys)

      val pnonces = tc.nonce_indices
        .map(i => MuSigNoncePub.fromBytes(vecs.pnonces(i)))
      val aggPnonce = MuSigNoncePub.fromBytes(vecs.aggnonces(tc.aggnonce_index))
      val msg = vecs.msgs(tc.msg_index)

      val secnonce = defaultSecNonce

      val s = MuSigUtil.sign(secnonce, aggPnonce, signerPriv, msg, keySet)
      assert(
        s == tc.expected,
        s"Failed to produce signature for test=${tc.comment.getOrElse("")}")

      // note when i remove the partialSigVerify, we produce the expected S
      // value according to the test vector, but the signature fails to verify.
      // This indicates that there is a bug (likely around parity handling)
      // in partialSigVerify()
      val verify = MuSigUtil.partialSigVerify(s,
                                              pnonces,
                                              keySet,
                                              msg,
                                              signerIndex = tc.signer_index)
      assert(verify,
             s"Failed to verify signature for test=${tc.comment.getOrElse("")}")
    }

    // SIGN error tests
    vecs.sign_error_test_cases.foreach { etc =>
      intercept[Exception] {
        val keys =
          etc.key_indices.map(i => vecs.pubkeys(i).toPublicKey).toVector
        val keySet = UnsortedKeySet(keys)
        val aggPnonce =
          MuSigNoncePub.fromBytes(vecs.aggnonces(etc.aggnonce_index))
        MuSigNoncePub.fromBytes(vecs.aggnonces(etc.aggnonce_index))
        val msg = vecs.msgs(etc.msg_index)
        val secnonce = etc.secnonce_index match {
          case Some(idx) => MuSigNoncePriv.fromBytes(vecs.secnonces(idx))
          case None      => defaultSecNonce
        }
        MuSigUtil.sign(secnonce, aggPnonce, signerPriv, msg, keySet)
      }
    }

    // VERIFY fail test cases (invalid signatures)
    vecs.verify_fail_test_cases.foreach { tc =>
      val keys = tc.key_indices.map(i => vecs.pubkeys(i).toPublicKey).toVector
      val keySet = UnsortedKeySet(keys)
      val pnonces = tc.nonce_indices
        .map(i => MuSigNoncePub.fromBytes(vecs.pnonces(i)))
        .toVector
      val msg = vecs.msgs(tc.msg_index)
      val s = tc.sig

      assert(
        !MuSigUtil.partialSigVerify(s, pnonces, keySet, msg, tc.signer_index))
    }

    // VERIFY error test cases (invalid contributions)
    vecs.verify_error_test_cases.foreach { tc =>
      intercept[Exception] {
        val keys = tc.key_indices.map(i => vecs.pubkeys(i).toPublicKey).toVector
        val keySet = UnsortedKeySet(keys)
        val pnonces = tc.nonce_indices
          .map(i => MuSigNoncePub.fromBytes(vecs.pnonces(i)))
          .toVector
        val msg = vecs.msgs(tc.msg_index)
        val s = FieldElement.fromBytes(tc.sig)
        MuSigUtil.partialSigVerify(s, pnonces, keySet, msg, tc.signer_index)
      }
    }
  }

  it must "pass tweak_vectors.json" in {
    val fileName = "/musig2/tweak_vectors.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)

    val vecs = json.validate[Musig2Json.TweakVectors].get

    val signerPriv = ECPrivateKey.fromBytes(vecs.sk)

    // For each valid test case, construct keyset, apply tweaks and sign
    vecs.valid_test_cases.foreach { tc =>
      val keys = tc.key_indices.map(i => vecs.pubkeys(i).toPublicKey)
      val keySetBase = UnsortedKeySet(keys)

      val tweaksVec = tc.tweak_indices.map(i => vecs.tweaks(i))
      val musigTweaks = tweaksVec.zip(tc.is_xonly).map { case (feBytes, isX) =>
        // parse FieldElement here to allow invalid tweaks to be caught per-test
        val fe = FieldElement.fromBytes(feBytes)
        MuSigTweak(fe, isXOnlyT = isX)
      }

      val keySet = keySetBase.withTweaks(musigTweaks)

      // Build pnonces list and aggregate
      val pnonces = tc.nonce_indices.map(i => vecs.pnonces(i)).toVector
      val aggPnonce = MuSigNoncePub.aggregate(pnonces)

      val s =
        MuSigUtil.sign(vecs.secnonce, aggPnonce, signerPriv, vecs.msg, keySet)

      assert(s == tc.expected)
    }

    // For error cases, ensure exception is thrown
    vecs.error_test_cases.foreach { etc =>
      intercept[Exception] {
        val keys =
          etc.key_indices.map(i => vecs.pubkeys(i).toPublicKey).toVector
        val keySetBase = UnsortedKeySet(keys)
        val tweaksVec = etc.tweak_indices.map(i => vecs.tweaks(i)).toVector
        val musigTweaks =
          tweaksVec.zip(etc.is_xonly).map { case (feBytes, isX) =>
            val fe = FieldElement.fromBytes(feBytes)
            MuSigTweak(fe, isXOnlyT = isX)
          }
        val keySet = keySetBase.withTweaks(musigTweaks)
        val pnonces = etc.nonce_indices.map(i => vecs.pnonces(i)).toVector
        val aggPnonce = MuSigNoncePub.aggregate(pnonces)
        MuSigUtil.sign(vecs.secnonce, aggPnonce, signerPriv, vecs.msg, keySet)
      }
    }
  }

  it must "pass sig_agg_vectors.json" in {
    val fileName = "/musig2/sig_agg_vectors.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)

    val vecs = json.validate[Musig2Json.SigAggVectors].get

    // VALID test cases
    vecs.valid_test_cases.foreach { tc =>
      // Build keyset
      val keys = tc.key_indices.map(i => vecs.pubkeys(i).toPublicKey).toVector
      var keySet: KeySet = UnsortedKeySet(keys)

      // Apply tweaks if present
      if (tc.tweak_indices.nonEmpty) {
        val tweaks = tc.tweak_indices.map(i => vecs.tweaks(i)).toVector
        val isXonly = tc.is_xonly
        val musigTweaks = tweaks.zip(isXonly).map { case (feBytes, isX) =>
          val fe = FieldElement.fromBytes(feBytes)
          MuSigTweak(fe, isXOnlyT = isX)
        }
        keySet = keySet.withTweaks(musigTweaks)
      }

      // Collect partial signatures referenced by psig_indices
      val sVals: Vector[FieldElement] = tc.psig_indices.map { idx =>
        FieldElement.fromBytes(vecs.psigs(idx))
      }.toVector

      // Aggregate the provided aggnonce bytes into MuSigNoncePub then get ECPublicKey
      val aggPnonce = MuSigNoncePub.fromBytes(tc.aggnonce)

      // Use signAgg that takes ECPublicKey
      val signature = MuSigUtil.signAgg(sVals, aggPnonce, keySet, vecs.msg)

      assert(signature == tc.expected,
             s"Sig agg test failed for comment=${tc.comment.getOrElse("")}")
    }

    // Error test cases
    vecs.error_test_cases.foreach { etc =>
      intercept[Exception] {
        val keys =
          etc.key_indices.map(i => vecs.pubkeys(i).toPublicKey).toVector
        val keySet = UnsortedKeySet(keys)
        val sVals = etc.psig_indices
          .map(idx => FieldElement.fromBytes(vecs.psigs(idx)))
          .toVector
        val aggPnonce = MuSigNoncePub.fromBytes(etc.aggnonce)
        // attempt to aggregate into signature - expecting failure per test
        MuSigUtil.signAgg(sVals, aggPnonce, keySet, vecs.msg)
      }
    }
  }

}
