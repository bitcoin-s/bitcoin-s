package org.bitcoins.keymanager.util

import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}
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
      case (SegWit, MainNet)                  => SegWitMainNetPriv
      case (SegWit, TestNet3 | RegTest)       => SegWitTestNet3Priv
      case (NestedSegWit, MainNet)            => NestedSegWitMainNetPriv
      case (NestedSegWit, TestNet3 | RegTest) => NestedSegWitTestNet3Priv
      case (Legacy, MainNet)                  => LegacyMainNetPriv
      case (Legacy, TestNet3 | RegTest)       => LegacyTestNet3Priv
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
      case (SegWit, MainNet)                  => SegWitMainNetPub
      case (SegWit, TestNet3 | RegTest)       => SegWitTestNet3Pub
      case (NestedSegWit, MainNet)            => NestedSegWitMainNetPub
      case (NestedSegWit, TestNet3 | RegTest) => NestedSegWitTestNet3Pub
      case (Legacy, MainNet)                  => LegacyMainNetPub
      case (Legacy, TestNet3 | RegTest)       => LegacyTestNet3Pub
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
