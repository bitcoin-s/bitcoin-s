package org.bitcoins.keymanager.util

import org.bitcoins.core.config.{MainNet, TestNet3}
import org.bitcoins.core.crypto.ExtKeyVersion.{
  LegacyMainNetPriv,
  LegacyMainNetPub,
  LegacyTestNet3Priv,
  LegacyTestNet3Pub,
  NestedSegWitMainNetPriv,
  NestedSegWitMainNetPub,
  NestedSegWitTestNet3Priv,
  NestedSegWitTestNet3Pub,
  SegWitMainNetPriv,
  SegWitMainNetPub,
  SegWitTestNet3Priv,
  SegWitTestNet3Pub
}
import org.bitcoins.core.hd.{HDCoinType, HDPurpose, HDPurposes}
import org.bitcoins.keymanager.KeyManagerUnitTest

class HdUtilTest extends KeyManagerUnitTest {

  it must "get the correct version for a public key" in {
    assert(
      HDUtil.getXpubVersion(HDPurposes.Legacy, MainNet) == LegacyMainNetPub)
    assert(
      HDUtil.getXpubVersion(HDPurposes.Legacy, TestNet3) == LegacyTestNet3Pub)
    assert(
      HDUtil.getXpubVersion(HDPurposes.SegWit, MainNet) == SegWitMainNetPub)
    assert(
      HDUtil.getXpubVersion(HDPurposes.SegWit, TestNet3) == SegWitTestNet3Pub)
    assert(
      HDUtil.getXpubVersion(HDPurposes.NestedSegWit, MainNet) == NestedSegWitMainNetPub)
    assert(
      HDUtil.getXpubVersion(HDPurposes.NestedSegWit, TestNet3) == NestedSegWitTestNet3Pub)

    assertThrows[IllegalArgumentException] {
      HDUtil.getXpubVersion(HDPurpose(-1), TestNet3)
    }
  }

  it must "get the correct version for a private key" in {
    assert(
      HDUtil.getXprivVersion(HDPurposes.Legacy, MainNet) == LegacyMainNetPriv)
    assert(
      HDUtil.getXprivVersion(HDPurposes.Legacy, TestNet3) == LegacyTestNet3Priv)
    assert(
      HDUtil.getXprivVersion(HDPurposes.SegWit, MainNet) == SegWitMainNetPriv)
    assert(
      HDUtil.getXprivVersion(HDPurposes.SegWit, TestNet3) == SegWitTestNet3Priv)
    assert(
      HDUtil.getXprivVersion(HDPurposes.NestedSegWit, MainNet) == NestedSegWitMainNetPriv)
    assert(
      HDUtil.getXprivVersion(HDPurposes.NestedSegWit, TestNet3) == NestedSegWitTestNet3Priv)

    assertThrows[IllegalArgumentException] {
      HDUtil.getXprivVersion(HDPurpose(-1), MainNet)
    }

    assertThrows[IllegalArgumentException] {
      HDUtil.getXprivVersion(HDPurpose(-1), TestNet3)
    }
  }

  it must "find the corresponding priv version for a pubkey" in {
    assert(
      HDUtil.getMatchingExtKeyVersion(LegacyMainNetPub) == LegacyMainNetPriv)
    assert(
      HDUtil.getMatchingExtKeyVersion(LegacyTestNet3Pub) == LegacyTestNet3Priv)
    assert(
      HDUtil.getMatchingExtKeyVersion(SegWitMainNetPub) == SegWitMainNetPriv)
    assert(
      HDUtil.getMatchingExtKeyVersion(SegWitTestNet3Pub) == SegWitTestNet3Priv)
    assert(
      HDUtil.getMatchingExtKeyVersion(NestedSegWitMainNetPub) == NestedSegWitMainNetPriv)
    assert(
      HDUtil.getMatchingExtKeyVersion(NestedSegWitTestNet3Pub) == NestedSegWitTestNet3Priv)
  }

  it must "find the corresponding pub version for a privkey" in {
    assert(
      HDUtil.getMatchingExtKeyVersion(LegacyMainNetPriv) == LegacyMainNetPub)
    assert(
      HDUtil.getMatchingExtKeyVersion(LegacyTestNet3Priv) == LegacyTestNet3Pub)
    assert(
      HDUtil.getMatchingExtKeyVersion(SegWitMainNetPriv) == SegWitMainNetPub)
    assert(
      HDUtil.getMatchingExtKeyVersion(SegWitTestNet3Priv) == SegWitTestNet3Pub)
    assert(
      HDUtil.getMatchingExtKeyVersion(NestedSegWitMainNetPriv) == NestedSegWitMainNetPub)
    assert(
      HDUtil.getMatchingExtKeyVersion(NestedSegWitTestNet3Priv) == NestedSegWitTestNet3Pub)
  }

  it must "get the right coin type" in {
    assert(HDUtil.getCoinType(MainNet) == HDCoinType.Bitcoin)
    assert(HDUtil.getCoinType(TestNet3) == HDCoinType.Testnet)
  }
}
