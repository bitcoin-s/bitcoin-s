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
    import org.bitcoins.core.crypto.ExtKeyVersion._
    import org.bitcoins.core.hd.HDPurposes._

    (hdPurpose, network) match {
      case (SegWit, MainNet | SigNet)            => SegWitMainNetPub
      case (SegWit, TestNet3 | RegTest | SigNet) => SegWitTestNet3Pub
      case (NestedSegWit, MainNet)               => NestedSegWitMainNetPub
      case (NestedSegWit, TestNet3 | RegTest | SigNet) =>
        NestedSegWitTestNet3Pub
      case (Multisig, MainNet) => LegacyMainNetPub
      case (Multisig, TestNet3 | RegTest | SigNet) =>
        LegacyTestNet3Pub
      case (Legacy, MainNet)                     => LegacyMainNetPub
      case (Legacy, TestNet3 | RegTest | SigNet) => LegacyTestNet3Pub
      case (unknown: HDPurpose, _) =>
        throw new IllegalArgumentException(s"Got unknown HD purpose $unknown")
    }
  }

  /** Gets the matching xpriv version to this xpub version */
  def getMatchingExtKeyVersion(version: ExtKeyPubVersion): ExtKeyPrivVersion = {
    import org.bitcoins.core.crypto.ExtKeyVersion._
    version match {
      case LegacyMainNetPub        => LegacyMainNetPriv
      case LegacyTestNet3Pub       => LegacyTestNet3Priv
      case NestedSegWitMainNetPub  => NestedSegWitMainNetPriv
      case NestedSegWitTestNet3Pub => NestedSegWitTestNet3Priv
      case SegWitMainNetPub        => SegWitMainNetPriv
      case SegWitTestNet3Pub       => SegWitTestNet3Priv
    }
  }

  /** Gets the matching xpub version to this xpriv version */
  def getMatchingExtKeyVersion(version: ExtKeyPrivVersion): ExtKeyPubVersion = {
    import org.bitcoins.core.crypto.ExtKeyVersion._
    version match {
      case LegacyMainNetPriv        => LegacyMainNetPub
      case LegacyTestNet3Priv       => LegacyTestNet3Pub
      case NestedSegWitMainNetPriv  => NestedSegWitMainNetPub
      case NestedSegWitTestNet3Priv => NestedSegWitTestNet3Pub
      case SegWitMainNetPriv        => SegWitMainNetPub
      case SegWitTestNet3Priv       => SegWitTestNet3Pub
    }
  }

  def getCoinType(network: NetworkParameters): HDCoinType = {
    HDCoinType.fromNetwork(network)
  }
}
