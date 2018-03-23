package org.bitcoins.core.crypto

import java.math.BigInteger

import org.bitcoin.NativeSecp256k1
import org.bitcoins.core.number.{ UInt32, UInt8 }
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util._

import scala.util.{ Failure, Success, Try }

/**
 * Represents an extended key as defined by BIP32
 * [[https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki]]
 */
sealed abstract class ExtKey extends NetworkElement {
  require(bytes.size == 78, "ExtKey must be 78 bytes in size, got: " + bytes.size)
  /** The network and private/public key identifier for this key */
  def version: ExtKeyVersion
  /** 0 for master nodes, 1 for level-1 derived keys, .... */
  def depth: UInt8
  /** The fingerprint of the parent key */
  def fingerprint: Seq[Byte]
  /**
   * Child number. This is ser32(i) for i in xi = xpar/i, with xi the key being serialized.
   * (0x00000000 if master key)
   */
  def childNum: UInt32

  /**
   * In order to prevent these from depending solely on the key itself,
   * we extend both private and public keys first with an extra 256 bits of entropy.
   * This extension, called the chain code,
   * is identical for corresponding private and public keys, and consists of 32 bytes.
   */
  def chainCode: ChainCode

  /** The key at this path */
  def key: BaseECKey

  def deriveChildPubKey(idx: UInt32): Try[ExtPublicKey] = this match {
    case priv: ExtPrivateKey =>
      Success(priv.deriveChildPrivKey(idx).extPublicKey)
    case pub: ExtPublicKey => pub.deriveChildPubKey(idx)
  }

  def deriveChildPubKey(idx: Long): Try[ExtPublicKey] = {
    Try(UInt32(idx)).flatMap(deriveChildPubKey(_))
  }

  override def bytes: Seq[Byte] = key match {
    case priv: ECPrivateKey =>
      version.bytes ++ depth.bytes ++ fingerprint ++
        childNum.bytes ++ chainCode.bytes ++ Seq(0.toByte) ++ priv.bytes
    case pub: ECPublicKey =>
      version.bytes ++ depth.bytes ++ fingerprint ++
        childNum.bytes ++ chainCode.bytes ++ pub.bytes
  }

  override def toString: String = {
    val checksum = CryptoUtil.doubleSHA256(bytes).bytes.take(4)
    val encoded = Base58.encode(bytes ++ checksum)
    require(Base58.decodeCheck(encoded).isSuccess)
    encoded
  }
}

object ExtKey extends Factory[ExtKey] {
  val hardenedIdx = UInt32(NumberUtil.pow2(31).toLong)

  /** Takes in a base58 string and tries to convert it to an extended key */
  def fromString(base58: String): Try[ExtKey] = {
    val decoded: Try[Seq[Byte]] = Base58.decodeCheck(base58)
    val extKey = decoded.flatMap { bytes =>
      require(bytes.size == 78, "Not 78 bytes")
      val version: Try[ExtKeyVersion] = ExtKeyVersion(bytes.take(4)) match {
        case Some(v) => Success(v)
        case None    => Failure(new IllegalArgumentException("Invalid version for ExtKey"))
      }
      val depth = UInt8(bytes.slice(4, 5))
      val fp = bytes.slice(5, 9)
      val childNum = UInt32(bytes.slice(9, 13))
      val chainCode = ChainCode(bytes.slice(13, 45))
      val key: Try[ExtKey] = version.map {
        case x @ (MainNetPub | TestNet3Pub) =>
          val pub = ECPublicKey(bytes.slice(45, 78))
          ExtPublicKey(x, depth, fp, childNum, chainCode, pub)
        case x @ (MainNetPriv | TestNet3Priv) =>
          require(bytes(45) == 0, "Byte at index 46 must be zero for a ExtPrivateKey, got: " + BitcoinSUtil.encodeHex(bytes(45)))
          val priv = ECPrivateKey(bytes.slice(46, 78))
          ExtPrivateKey(x, depth, fp, childNum, chainCode, priv)
      }
      key
    }
    extKey
  }

  override def fromBytes(bytes: Seq[Byte]): ExtKey = {
    val privTry = Try(ExtPrivateKey(bytes))
    if (privTry.isSuccess) privTry.get
    else {
      ExtPublicKey(bytes)
    }
  }
}

sealed abstract class ExtPrivateKey extends ExtKey {
  override def key: ECPrivateKey

  def deriveChildPrivKey(idx: UInt32): ExtPrivateKey = {
    val data: Seq[Byte] = if (idx >= ExtKey.hardenedIdx) {
      //derive hardened key
      0.toByte +: ((key.bytes) ++ idx.bytes)
    } else {
      //derive non hardened key
      key.publicKey.bytes ++ idx.bytes
    }
    val hmac = CryptoUtil.hmac512(chainCode.bytes, data)
    val (il, ir) = hmac.splitAt(32)
    //should be ECGroup addition
    //parse256(IL) + kpar (mod n)
    val childKey = ECPrivateKey(NativeSecp256k1.privKeyTweakAdd(il.toArray, key.bytes.toArray))
    val fp = CryptoUtil.sha256Hash160(key.publicKey.bytes).bytes.take(4)
    ExtPrivateKey(version, depth + UInt8.one, fp, idx,
      ChainCode(ir), childKey)
  }

  def extPublicKey: ExtPublicKey = version match {
    case MainNetPriv              => ExtPublicKey(MainNetPub, depth, fingerprint, childNum, chainCode, key.publicKey)
    case TestNet3Priv             => ExtPublicKey(TestNet3Pub, depth, fingerprint, childNum, chainCode, key.publicKey)
    case MainNetPub | TestNet3Pub => throw new IllegalArgumentException("Cannot have pubkey version in ExtPrivateKey, got: " + version)
  }

  def deriveChildPrivKey(idx: Long): Try[ExtPrivateKey] = {
    Try(UInt32(idx)).map(deriveChildPrivKey(_))
  }
}
object ExtPrivateKey extends Factory[ExtPrivateKey] {
  private case class ExtPrivateKeyImpl(version: ExtKeyVersion, depth: UInt8,
                                       fingerprint: Seq[Byte], childNum: UInt32,
                                       chainCode: ChainCode, key: ECPrivateKey) extends ExtPrivateKey {
    require(fingerprint.size == 4, "Fingerprint must be 4 bytes in size, got: " + fingerprint)
  }

  override def fromBytes(bytes: Seq[Byte]): ExtPrivateKey = {
    require(bytes.size == 78, "ExtPrivateKey can only be 78 bytes")
    val base58 = Base58.encode(bytes ++ CryptoUtil.doubleSHA256(bytes).bytes.take(4))
    ExtKey.fromString(base58) match {
      case Success(priv: ExtPrivateKey) => priv
      case Success(_: ExtPublicKey)     => throw new IllegalArgumentException("Cannot create ext public in ExtPrivateKey")
      case f: Failure[_]                => throw f.exception
    }
  }
  def apply(version: ExtKeyVersion, depth: UInt8,
            fingerprint: Seq[Byte], child: UInt32,
            chainCode: ChainCode, privateKey: ECPrivateKey): ExtPrivateKey = {
    ExtPrivateKeyImpl(version, depth, fingerprint, child, chainCode, privateKey)
  }

  /**
   * Generates a master private key
   * https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#master-key-generation
   */
  def apply(version: ExtKeyVersion, seedOpt: Option[Seq[Byte]] = None): ExtPrivateKey = {
    val seed = seedOpt match {
      case Some(bytes) => bytes
      case None        => ECPrivateKey().bytes
    }
    val i = CryptoUtil.hmac512("Bitcoin seed".map(_.toByte), seed)
    val (il, ir) = i.splitAt(32)
    val masterPrivKey = ECPrivateKey(il)
    val fp = UInt32.zero.bytes
    ExtPrivateKey(version, UInt8.zero, fp, UInt32.zero,
      ChainCode(ir), masterPrivKey)
  }
}

sealed abstract class ExtPublicKey extends ExtKey {
  override def key: ECPublicKey

  final override def deriveChildPubKey(idx: UInt32): Try[ExtPublicKey] = {
    if (idx >= ExtKey.hardenedIdx) {
      Failure(new IllegalArgumentException("Cannot derive hardened child from extended public key"))
    } else {
      val data = key.bytes ++ idx.bytes
      val hmac = CryptoUtil.hmac512(chainCode.bytes, data)
      val (il, ir) = hmac.splitAt(32)
      val priv = ECPrivateKey(il)
      val tweaked = NativeSecp256k1.pubKeyTweakAdd(
        key.bytes.toArray,
        hmac.toArray,
        priv.isCompressed
      )
      val childPubKey = ECPublicKey(tweaked)
      val bi = BigInt(new BigInteger(1, priv.bytes.toArray))
      //we do not handle this case since it is impossible
      //In case parse256(IL) ≥ n or Ki is the point at infinity, the resulting key is invalid,
      //and one should proceed with the next value for i.
      //https://botbot.me/freenode/bitcoin-wizards/2017-11-20/?tz=America/Chicago
      val cc = ChainCode(ir)
      val fp = CryptoUtil.sha256Hash160(key.bytes).bytes.take(4)
      Success(ExtPublicKey(version, depth + UInt8.one, fp, idx, cc, childPubKey))
    }
  }
}

object ExtPublicKey extends Factory[ExtPublicKey] {
  private case class ExtPublicKeyImpl(version: ExtKeyVersion, depth: UInt8,
                                      fingerprint: Seq[Byte], childNum: UInt32,
                                      chainCode: ChainCode, key: ECPublicKey) extends ExtPublicKey

  def apply(version: ExtKeyVersion, depth: UInt8,
            fingerprint: Seq[Byte], child: UInt32, chainCode: ChainCode, publicKey: ECPublicKey): ExtPublicKey = {
    ExtPublicKeyImpl(version, depth, fingerprint, child, chainCode, publicKey)
  }

  override def fromBytes(bytes: Seq[Byte]): ExtPublicKey = {
    require(bytes.size == 78, "ExtPublicKey can only be 78 bytes")
    val base58 = Base58.encode(bytes ++ CryptoUtil.doubleSHA256(bytes).bytes.take(4))
    ExtKey.fromString(base58) match {
      case Success(_: ExtPrivateKey) =>
        throw new IllegalArgumentException("Cannot create ext privatkey in ExtPublicKey")
      case Success(pub: ExtPublicKey) => pub
      case f: Failure[_]              => throw f.exception
    }
  }
}

