package org.bitcoins.core.crypto

import org.bitcoin.NativeSecp256k1
import org.bitcoins.core.hd.{BIP32Node, BIP32Path}
import org.bitcoins.core.number.{UInt32, UInt8}
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util._
import scodec.bits.{ByteVector, HexStringSyntax}

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/**
  * Represents an extended key as defined by BIP32
  * [[https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki]]
  */
sealed abstract class ExtKey extends NetworkElement {
  require(bytes.size == 78,
          "ExtKey must be 78 bytes in size, got: " + bytes.size)

  protected type VersionType <: ExtKeyVersion

  /** The network and private/public key identifier for this key */
  def version: VersionType

  /** 0 for master nodes, 1 for level-1 derived keys, .... */
  def depth: UInt8

  /** The fingerprint of the parent key */
  def fingerprint: ByteVector

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

  /**
    * Derives the child pubkey at the specified index
    */
  def deriveChildPubKey(idx: UInt32): Try[ExtPublicKey] = this match {
    case priv: ExtPrivateKey =>
      Success(priv.deriveChildPrivKey(idx).extPublicKey)
    case pub: ExtPublicKey => pub.deriveChildPubKey(idx)
  }

  /**
    * Derives the child pubkey at the specified index
    */
  def deriveChildPubKey(idx: Long): Try[ExtPublicKey] = {
    Try(UInt32(idx)).flatMap(deriveChildPubKey)
  }

  /**
    * Derives the child pubkey at the specified index and
    * hardening value
    */
  def deriveChildPubKey(child: BIP32Node): Try[ExtPublicKey] = {
    deriveChildPubKey(child.toUInt32)
  }

  /**
    * Derives the child pubkey at the specified path
    */
  def deriveChildPubKey(path: BIP32Path): Try[ExtPublicKey] = {
    this match {
      case priv: ExtPrivateKey =>
        Success(priv.deriveChildPrivKey(path).extPublicKey)
      case pub: ExtPublicKey =>
        @tailrec
        def loop(
            remainingPath: List[BIP32Node],
            accum: ExtPublicKey): Try[ExtPublicKey] = {
          remainingPath match {
            case h :: t =>
              accum.deriveChildPubKey(h) match {
                case Success(derivedPub) => loop(t, derivedPub)
                case failure: Failure[_] => failure
              }
            case Nil => Success(accum)
          }
        }
        loop(path.path.toList, pub)
    }

  }

  override def bytes: ByteVector = key match {
    case priv: ECPrivateKey =>
      version.bytes ++ depth.bytes ++ fingerprint ++
        childNum.bytes ++ chainCode.bytes ++ ByteVector.low(1) ++ priv.bytes
    case pub: ECPublicKey =>
      version.bytes ++ depth.bytes ++ fingerprint ++
        childNum.bytes ++ chainCode.bytes ++ pub.bytes
  }

  override def toString: String = {
    ExtKey.toString(this)
  }

}

object ExtKey extends Factory[ExtKey] {
  val hardenedIdx = UInt32(NumberUtil.pow2(31).toLong)

  val masterFingerprint: ByteVector = hex"00000000"

  /** Takes in a base58 string and tries to convert it to an extended key */
  def fromString(base58: String): Try[ExtKey] = {
    val decoded: Try[ByteVector] = Base58.decodeCheck(base58)
    val extKey = decoded.flatMap { bytes =>
      require(bytes.size == 78, "Not 78 bytes")
      val version: Try[ExtKeyVersion] = ExtKeyVersion(bytes.take(4)) match {
        case Some(v) => Success(v)
        case None =>
          Failure(new IllegalArgumentException("Invalid version for ExtKey"))
      }
      val depth = UInt8(bytes.slice(4, 5))
      val fp = bytes.slice(5, 9)
      val childNum = UInt32(bytes.slice(9, 13))
      val chainCode = ChainCode(bytes.slice(13, 45))
      val key: Try[ExtKey] = version.map {
        case x: ExtKeyPubVersion =>
          val pub = ECPublicKey(bytes.slice(45, 78))
          ExtPublicKey(x, depth, fp, childNum, chainCode, pub)
        case x: ExtKeyPrivVersion =>
          require(
            bytes(45) == 0,
            "Byte at index 46 must be zero for a ExtPrivateKey, got: " + BitcoinSUtil
              .encodeHex(bytes(45)))
          val priv = ECPrivateKey(bytes.slice(46, 78))
          ExtPrivateKey(x, depth, fp, childNum, chainCode, priv)
      }
      key
    }
    extKey
  }

  def toString(extKey: ExtKey): String = {
    val bytes = extKey.bytes
    val checksum = CryptoUtil.doubleSHA256(bytes).bytes.take(4)
    val encoded = Base58.encode(bytes ++ checksum)
    require(Base58.decodeCheck(encoded).isSuccess)
    encoded
  }

  override def fromBytes(bytes: ByteVector): ExtKey = {
    val privTry = Try(ExtPrivateKey(bytes))
    if (privTry.isSuccess) privTry.get
    else {
      ExtPublicKey(bytes)
    }
  }
}

sealed abstract class ExtPrivateKey
    extends ExtKey
    with ExtSign
    with MaskedToString {
  import ExtKeyVersion._

  override protected type VersionType = ExtKeyPrivVersion

  override def key: ECPrivateKey

  /**
    * Derives the child key corresponding to the given path. The given path
    * could signify account levels, one sublevel for each currency, or
    * how to derive change addresses.
    *
    * @see [[org.bitcoins.core.hd.HDPath HDPath]] for a more
    *      specialized version of a BIP32 path
    */
  def deriveChildPrivKey(path: BIP32Path): ExtPrivateKey = {
    path.path.foldLeft(this)((accum: ExtPrivateKey, curr: BIP32Node) =>
      accum.deriveChildPrivKey(curr.toUInt32))
  }

  def deriveChildPrivKey(idx: UInt32): ExtPrivateKey = {
    val data: ByteVector = if (idx >= ExtKey.hardenedIdx) {
      //derive hardened key
      hex"0" ++ key.bytes ++ idx.bytes
    } else {
      //derive non hardened key
      key.publicKey.bytes ++ idx.bytes
    }
    val hmac = CryptoUtil.hmac512(chainCode.bytes, data)
    val (il, ir) = hmac.splitAt(32)
    //should be ECGroup addition
    //parse256(IL) + kpar (mod n)
    val tweak = NativeSecp256k1.privKeyTweakAdd(il.toArray, key.bytes.toArray)
    val childKey = ECPrivateKey(ByteVector(tweak))
    val fp = CryptoUtil.sha256Hash160(key.publicKey.bytes).bytes.take(4)
    ExtPrivateKey(version, depth + UInt8.one, fp, idx, ChainCode(ir), childKey)
  }

  def extPublicKey: ExtPublicKey = {
    val pub = version match {
      case SegWitMainNetPriv        => SegWitMainNetPub
      case SegWitTestNet3Priv       => SegWitTestNet3Pub
      case NestedSegWitMainNetPriv  => NestedSegWitMainNetPub
      case NestedSegWitTestNet3Priv => NestedSegWitTestNet3Pub
      case LegacyMainNetPriv        => LegacyMainNetPub
      case LegacyTestNet3Priv       => LegacyTestNet3Pub
    }
    ExtPublicKey(pub, depth, fingerprint, childNum, chainCode, key.publicKey)
  }

  def deriveChildPrivKey(idx: Long): Try[ExtPrivateKey] = {
    Try(UInt32(idx)).map(deriveChildPrivKey)
  }

  override def publicKey: ECPublicKey = key.publicKey

  override def signFunction: ByteVector => Future[ECDigitalSignature] = {
    key.signFunction
  }

  /** Signs the given bytes with the given [[BIP32Path path]] */
  override def deriveAndSignFuture: (
      ByteVector,
      BIP32Path) => Future[ECDigitalSignature] = {
    case (bytes, path) =>
      deriveChildPrivKey(path).signFunction(bytes)
  }

  override def toStringSensitive: String = {
    ExtKey.toString(this)
  }
}

object ExtPrivateKey extends Factory[ExtPrivateKey] {
  private case class ExtPrivateKeyImpl(
      version: ExtKeyPrivVersion,
      depth: UInt8,
      fingerprint: ByteVector,
      childNum: UInt32,
      chainCode: ChainCode,
      key: ECPrivateKey)
      extends ExtPrivateKey {
    require(fingerprint.size == 4,
            "Fingerprint must be 4 bytes in size, got: " + fingerprint)
  }

  def freshRootKey(version: ExtKeyPrivVersion): ExtPrivateKey = {
    val privKey = ECPrivateKey.freshPrivateKey
    val chainCode = ChainCode.fromBytes(ECPrivateKey.freshPrivateKey.bytes)

    ExtPrivateKey(
      version,
      UInt8.zero,
      UInt32.zero.bytes,
      UInt32.zero,
      chainCode,
      privKey
    )
  }

  /** Takes in a base58 string and tries to convert it to an extended private key */
  def fromString(base58: String): Try[ExtPrivateKey] =
    ExtKey.fromString(base58) match {
      case Success(priv: ExtPrivateKey) => Success(priv)
      case Success(_: ExtPublicKey) =>
        Failure(
          new IllegalArgumentException(
            "Got extended public key, expected private"))
      // we get warnings about unchecked generics
      // if we do fail: Failure[ExtPrivateKey] and
      // compile error if we do fail: Failure[_]
      case Failure(exc) => Failure(exc)
    }

  override def fromBytes(bytes: ByteVector): ExtPrivateKey = {
    require(bytes.size == 78, "ExtPrivateKey can only be 78 bytes")
    val base58 =
      Base58.encode(bytes ++ CryptoUtil.doubleSHA256(bytes).bytes.take(4))
    ExtKey.fromString(base58) match {
      case Success(priv: ExtPrivateKey) => priv
      case Success(_: ExtPublicKey) =>
        throw new IllegalArgumentException(
          "Cannot create ext public in ExtPrivateKey")
      case f: Failure[_] => throw f.exception
    }
  }

  def apply(
      version: ExtKeyPrivVersion,
      depth: UInt8,
      fingerprint: ByteVector,
      child: UInt32,
      chainCode: ChainCode,
      privateKey: ECPrivateKey): ExtPrivateKey = {
    ExtPrivateKeyImpl(version, depth, fingerprint, child, chainCode, privateKey)
  }

  /**
    * Hard coded value according to
    * [[https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#master-key-generation BIP32]]
    */
  private val BIP32_KEY: ByteVector =
    ByteVector.encodeAscii("Bitcoin seed") match {
      case Left(exception) => throw exception
      case Right(bytevec)  => bytevec
    }

  /**
    * Generates a master private key
    * https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#master-key-generation
    */
  def apply(
      version: ExtKeyPrivVersion,
      seedOpt: Option[ByteVector] = None,
      path: BIP32Path = BIP32Path.empty): ExtPrivateKey = {
    val seed: ByteVector = seedOpt match {
      case Some(bytes) => bytes
      case None        => ECPrivateKey().bytes
    }
    val i =
      CryptoUtil.hmac512(key = BIP32_KEY, data = seed)
    val (masterPrivBytes, chaincodeBytes) = i.splitAt(32)
    val masterPrivKey = ECPrivateKey(masterPrivBytes)
    val chaincode = ChainCode(chaincodeBytes)
    val fingerprint = UInt32.zero.bytes
    val root = ExtPrivateKey(version,
                             depth = UInt8.zero,
                             fingerprint = fingerprint,
                             child = UInt32.zero,
                             chaincode,
                             masterPrivKey)

    path.path.foldLeft(root)((accum, curr) =>
      accum.deriveChildPrivKey(curr.toUInt32))
  }

  /** Generates a extended private key from the provided seed and version */
  def fromBIP39Seed(
      version: ExtKeyPrivVersion,
      seed: BIP39Seed,
      path: BIP32Path = BIP32Path.empty) =
    ExtPrivateKey(version, Some(seed.bytes), path)
}

sealed abstract class ExtPublicKey extends ExtKey {
  override protected type VersionType = ExtKeyPubVersion

  override def key: ECPublicKey

  final override def deriveChildPubKey(idx: UInt32): Try[ExtPublicKey] = {
    if (idx >= ExtKey.hardenedIdx) {
      Failure(
        new IllegalArgumentException(
          "Cannot derive hardened child from extended public key"))
    } else {
      val data = key.bytes ++ idx.bytes
      val hmac = CryptoUtil.hmac512(chainCode.bytes, data)
      val (il, ir) = hmac.splitAt(32)
      val priv = ECPrivateKey(il)
      val tweaked = NativeSecp256k1.pubKeyTweakAdd(key.bytes.toArray,
                                                   hmac.toArray,
                                                   priv.isCompressed)
      val childPubKey = ECPublicKey(ByteVector(tweaked))

      //we do not handle this case since it is impossible
      //In case parse256(IL) ≥ n or Ki is the point at infinity, the resulting key is invalid,
      //and one should proceed with the next value for i.
      //https://botbot.me/freenode/bitcoin-wizards/2017-11-20/?tz=America/Chicago
      val cc = ChainCode(ir)
      val fp = CryptoUtil.sha256Hash160(key.bytes).bytes.take(4)
      Success(
        ExtPublicKey(version, depth + UInt8.one, fp, idx, cc, childPubKey))
    }
  }
}

object ExtPublicKey extends Factory[ExtPublicKey] {
  private case class ExtPublicKeyImpl(
      version: ExtKeyPubVersion,
      depth: UInt8,
      fingerprint: ByteVector,
      childNum: UInt32,
      chainCode: ChainCode,
      key: ECPublicKey)
      extends ExtPublicKey

  def apply(
      version: ExtKeyPubVersion,
      depth: UInt8,
      fingerprint: ByteVector,
      child: UInt32,
      chainCode: ChainCode,
      publicKey: ECPublicKey): ExtPublicKey = {
    ExtPublicKeyImpl(version, depth, fingerprint, child, chainCode, publicKey)
  }

  /** Takes in a base58 string and tries to convert it to an extended public key */
  def fromString(base58: String): Try[ExtPublicKey] =
    ExtKey.fromString(base58) match {
      case Success(pub: ExtPublicKey) => Success(pub)
      case Success(_: ExtPrivateKey) =>
        Failure(
          new IllegalArgumentException(
            "Got extended private key, expected public"))
      // we get warnings about unchecked generics
      // if we do fail: Failure[ExtPublicKey] and
      // compile error if we do fail: Failure[_]
      case Failure(fail) => Failure(fail)
    }

  override def fromBytes(bytes: ByteVector): ExtPublicKey = {
    require(bytes.size == 78, "ExtPublicKey can only be 78 bytes")
    val base58 =
      Base58.encode(bytes ++ CryptoUtil.doubleSHA256(bytes).bytes.take(4))
    ExtKey.fromString(base58) match {
      case Success(_: ExtPrivateKey) =>
        throw new IllegalArgumentException(
          "Cannot create ext privatkey in ExtPublicKey")
      case Success(pub: ExtPublicKey) => pub
      case f: Failure[_]              => throw f.exception
    }
  }
}
