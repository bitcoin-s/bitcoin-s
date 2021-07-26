package org.bitcoins.core.util

import org.bitcoins.core.config._
import org.bitcoins.core.crypto.{ExtKeyPrivVersion, ExtKeyPubVersion}
import org.bitcoins.core.hd.{HDCoinType, HDPurpose}

object HDUtil {

  /** Gets the xpriv version required for the given HD purpose */
  def getXprivVersion(
      hdPurpose: HDPurpose,
      network: NetworkParameters): ExtKeyPrivVersion = {
    import org.bitcoins.core.crypto.ExtKeyVersion._
    import org.bitcoins.core.hd.HDPurposes._

    (hdPurpose, network) match {
      case (SegWit, MainNet)                     => SegWitMainNetPriv
      case (SegWit, TestNet3 | RegTest | SigNet) => SegWitTestNet3Priv
      case (NestedSegWit, MainNet)               => NestedSegWitMainNetPriv
      case (NestedSegWit, TestNet3 | RegTest | SigNet) =>
        NestedSegWitTestNet3Priv
      case (Multisig, MainNet) => LegacyMainNetPriv
      case (Multisig, TestNet3 | RegTest | SigNet) =>
        LegacyTestNet3Priv
      case (Legacy, MainNet)                             => LegacyMainNetPriv
      case (Legacy, TestNet3 | RegTest | SigNet)         => LegacyTestNet3Priv
      case (HDPurpose(585), MainNet)                     => SegWitMainNetPriv
      case (HDPurpose(585), TestNet3 | RegTest | SigNet) => SegWitTestNet3Priv
      case (unknown: HDPurpose, _) =>
        throw new IllegalArgumentException(s"Got unknown HD purpose $unknown")
    }
  }

  /** Gets the xpub version required for the given HD purpose */
  def getXpubVersion(
      hdPurpose: HDPurpose,
      network: NetworkParameters): ExtKeyPubVersion = {
    import org.bitcoins.core.hd.HDPurposes._

    (hdPurpose, network) match {
      case (SegWit, MainNet | SigNet) => ExtKeyPubVersion.SegWitMainNetPub
      case (SegWit, TestNet3 | RegTest | SigNet) =>
        ExtKeyPubVersion.SegWitTestNet3Pub
      case (NestedSegWit, MainNet) => ExtKeyPubVersion.NestedSegWitMainNetPub
      case (NestedSegWit, TestNet3 | RegTest | SigNet) =>
        ExtKeyPubVersion.NestedSegWitTestNet3Pub
      case (Multisig, MainNet) => ExtKeyPubVersion.LegacyMainNetPub
      case (Multisig, TestNet3 | RegTest | SigNet) =>
        ExtKeyPubVersion.LegacyTestNet3Pub
      case (Legacy, MainNet) => ExtKeyPubVersion.LegacyMainNetPub
      case (Legacy, TestNet3 | RegTest | SigNet) =>
        ExtKeyPubVersion.LegacyTestNet3Pub
      case (unknown: HDPurpose, _) =>
        throw new IllegalArgumentException(s"Got unknown HD purpose $unknown")
    }
  }

  /** Gets the matching xpriv version to this xpub version */
  def getMatchingExtKeyVersion(version: ExtKeyPubVersion): ExtKeyPrivVersion = {
    import org.bitcoins.core.crypto.ExtKeyVersion._
    version match {
      case ExtKeyPubVersion.LegacyMainNetPub        => LegacyMainNetPriv
      case ExtKeyPubVersion.LegacyTestNet3Pub       => LegacyTestNet3Priv
      case ExtKeyPubVersion.NestedSegWitMainNetPub  => NestedSegWitMainNetPriv
      case ExtKeyPubVersion.NestedSegWitTestNet3Pub => NestedSegWitTestNet3Priv
      case ExtKeyPubVersion.SegWitMainNetPub        => SegWitMainNetPriv
      case ExtKeyPubVersion.SegWitTestNet3Pub       => SegWitTestNet3Priv
    }
  }

  /** Gets the matching xpub version to this xpriv version */
  def getMatchingExtKeyVersion(version: ExtKeyPrivVersion): ExtKeyPubVersion = {
    import org.bitcoins.core.crypto.ExtKeyVersion._
    version match {
      case LegacyMainNetPriv        => ExtKeyPubVersion.LegacyMainNetPub
      case LegacyTestNet3Priv       => ExtKeyPubVersion.LegacyTestNet3Pub
      case NestedSegWitMainNetPriv  => ExtKeyPubVersion.NestedSegWitMainNetPub
      case NestedSegWitTestNet3Priv => ExtKeyPubVersion.NestedSegWitTestNet3Pub
      case SegWitMainNetPriv        => ExtKeyPubVersion.SegWitMainNetPub
      case SegWitTestNet3Priv       => ExtKeyPubVersion.SegWitTestNet3Pub
    }
  }

  def getCoinType(network: NetworkParameters): HDCoinType = {
    HDCoinType.fromNetwork(network)
  }
}
