package org.bitcoins.core.protocol

import org.bitcoins.core.config._
import org.bitcoins.core.number.{UInt5, UInt8}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.script.constant.ScriptConstant
import org.bitcoins.core.util.Bech32Encoding.Bech32m
import org.bitcoins.core.util._
import org.bitcoins.crypto._
import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}

sealed abstract class Address {

  /** The network that this address is valid for */
  def networkParameters: NetworkParameters

  /** The string representation of this address */
  def value: String

  override def equals(obj: Any): Boolean =
    obj match {
      case addr: Address => value == addr.value
      case _: Any        => false
    }

  /** Every address is derived from a [[HashDigest HashDigest]] in a
    * [[org.bitcoins.core.protocol.transaction.TransactionOutput TransactionOutput]]
    */
  def hash: HashDigest

  /** The [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]] the
    * address represents
    */
  def scriptPubKey: ScriptPubKey

  override def toString: String = value

  def descriptor: String = s"addr($value)"

  /** Checks if the address currently follows the standardness rules of bitcoin
    * and will be relayed by network.
    *
    * Currently this just means verifying the address is not of a
    * [[UnassignedWitnessScriptPubKey]]
    */
  def isStandard: Boolean
}

sealed abstract class BitcoinAddress extends Address

sealed abstract class P2PKHAddress extends BitcoinAddress {

  /** The base58 string representation of this address */
  override def value: String = {
    val versionByte = networkParameters.p2pkhNetworkByte
    val bytes = versionByte ++ hash.bytes
    val checksum = CryptoUtil.doubleSHA256(bytes).bytes.take(4)
    Base58.encode(bytes ++ checksum)
  }

  override def hash: Sha256Hash160Digest

  override def scriptPubKey: P2PKHScriptPubKey = P2PKHScriptPubKey(hash)

  override def isStandard: Boolean = true
}

sealed abstract class P2SHAddress extends BitcoinAddress {

  /** The base58 string representation of this address */
  override def value: String = {
    val versionByte = networkParameters.p2shNetworkByte
    val bytes = versionByte ++ hash.bytes
    val checksum = CryptoUtil.doubleSHA256(bytes).bytes.take(4)
    Base58.encode(bytes ++ checksum)
  }

  override def scriptPubKey: P2SHScriptPubKey = P2SHScriptPubKey(hash)

  override def hash: Sha256Hash160Digest

  override def isStandard: Boolean = true
}

/** https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki
  */
sealed abstract class Bech32Address extends BitcoinAddress {

  lazy val hrp: BtcHumanReadablePart = BtcHumanReadablePart(networkParameters)

  def data: Vector[UInt5]

  override def value: String = {
    val all: Vector[UInt5] = data ++ checksum
    val encoding = Bech32.encode5bitToString(all)

    hrp.toString + Bech32.separator + encoding
  }

  def checksum: Vector[UInt5] = Bech32Address.createChecksum(hrp, data)

  override def scriptPubKey: WitnessScriptPubKeyV0 = {
    Bech32Address.fromStringToWitSPK(value).get
  }

  override def hash: HashDigest = {
    val byteVector = BytesUtil.toByteVector(scriptPubKey.witnessProgram)
    scriptPubKey match {
      case _: P2WPKHWitnessSPKV0 =>
        Sha256Hash160Digest(byteVector)
      case _: P2WSHWitnessSPKV0 =>
        Sha256Digest(byteVector)
    }
  }

  def expandHrp: Vector[UInt5] = {
    hrp.expand
  }

  def verifyChecksum: Boolean = {
    Bech32.verifyChecksum(hrp.expand, data ++ checksum, Bech32Encoding.Bech32)
  }

  override def isStandard: Boolean = true
}

object Bech32Address extends AddressFactory[Bech32Address] {

  private case class Bech32AddressImpl(
      networkParameters: NetworkParameters,
      data: Vector[UInt5])
      extends Bech32Address {
    require(verifyChecksum, s"checksum did not pass $checksum")
  }

  def empty(network: NetworkParameters = MainNet): Bech32Address =
    fromScriptPubKey(P2WSHWitnessSPKV0(EmptyScriptPubKey), network)

  def apply(
      witSPK: WitnessScriptPubKey,
      networkParameters: NetworkParameters): Bech32Address = {
    // we don't encode the wit version or pushop for program into base5
    val prog = UInt8.toUInt8s(witSPK.asmBytes.tail.tail)
    val encoded = Bech32.from8bitTo5bit(prog)
    val witVersion = witSPK.witnessVersion.version.toInt.toByte
    Bech32Address(networkParameters, Vector(UInt5(witVersion)) ++ encoded)
  }

  def apply(
      networkParameters: NetworkParameters,
      data: Vector[UInt5]): Bech32Address = {
    Bech32AddressImpl(networkParameters, data)
  }

  /** Returns a base 5 checksum as specified by BIP173 */
  def createChecksum(
      hrp: BtcHumanReadablePart,
      bytes: Vector[UInt5]): Vector[UInt5] = {
    val values = hrp.expand ++ bytes
    Bech32.createChecksum(values, Bech32Encoding.Bech32)
  }

  /** Tries to convert the given string a to a
    * [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]]
    */
  def fromStringToWitSPK(string: String): Try[WitnessScriptPubKeyV0] = {
    val decoded = fromStringT(string)
    decoded.flatMap { bech32Addr =>
      val bytes = bech32Addr.data
      val (v, _) = (bytes.head, bytes.tail)
      val convertedProg = NumberUtil.convertUInt5sToUInt8(bytes.tail)
      val progBytes = UInt8.toBytes(convertedProg)
      val witVersion = WitnessVersion(v.toInt)
      val pushOp = BitcoinScriptUtil.calculatePushOp(progBytes)
      witVersion match {
        case Some(v) =>
          val witSPK = Try(
            WitnessScriptPubKeyV0(
              List(v.version) ++ pushOp ++ List(ScriptConstant(progBytes))))
          witSPK match {
            case Success(spk) => Success(spk)
            case Failure(err) =>
              Failure(
                new IllegalArgumentException(
                  "Failed to decode bech32 into a witSPK: " + err.getMessage))
          }
        case None =>
          Failure(
            new IllegalArgumentException(
              "Witness version was not valid, got: " + v))
      }
    }
  }

  /** Decodes bech32 string to the
    * [[org.bitcoins.core.protocol.BtcHumanReadablePart HumanReadablePart]] &
    * data part
    */
  override def fromString(bech32Str: String): Bech32Address = {
    val bech32T = for {
      (hrp, data) <- Bech32.splitToHrpAndData(bech32Str, Bech32Encoding.Bech32)
      network = BtcHumanReadablePart.fromString(hrp).network
    } yield Bech32Address(network, data)

    bech32T match {
      case Success(bech32) => bech32
      case Failure(exn)    => throw exn
    }
  }

  override def fromScriptPubKeyT(
      spk: ScriptPubKey,
      np: NetworkParameters): Try[Bech32Address] =
    spk match {
      case witSPK: WitnessScriptPubKeyV0 =>
        Success(Bech32Address(witSPK, np))
      case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
          _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
          _: P2SHScriptPubKey | _: LockTimeScriptPubKey |
          _: WitnessScriptPubKey | _: ConditionalScriptPubKey |
          _: NonStandardScriptPubKey | _: WitnessCommitment |
          _: UnassignedWitnessScriptPubKey | EmptyScriptPubKey) =>
        Failure(
          new IllegalArgumentException(
            "Cannot create a address for the scriptPubKey: " + x))
    }

}

/** https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki
  */
sealed abstract class Bech32mAddress extends BitcoinAddress {

  lazy val hrp: BtcHumanReadablePart = BtcHumanReadablePart(networkParameters)

  def data: Vector[UInt5]

  override def value: String = {
    val all: Vector[UInt5] = data ++ checksum
    val encoding = Bech32.encode5bitToString(all)

    hrp.toString + Bech32.separator + encoding
  }

  def checksum: Vector[UInt5] = Bech32mAddress.createChecksum(hrp, data)

  override def scriptPubKey: WitnessScriptPubKey = {
    val spk = Bech32mAddress.fromStringToWitSPK(value).get
    require(spk.witnessVersion != WitnessVersion0,
            "Use bech32 addresses for segwit v0")
    spk
  }

  override def hash: HashDigest = {
    DoubleSha256Digest.empty
  }

  def expandHrp: Vector[UInt5] = {
    hrp.expand
  }

  def verifyChecksum: Boolean = {
    Bech32.verifyChecksum(hrp.expand, data ++ checksum, Bech32Encoding.Bech32m)
  }

  override def isStandard: Boolean = {
    scriptPubKey match {
      case _: WitnessScriptPubKeyV0         => false // shouldn't be possible
      case _: TaprootScriptPubKey           => true
      case _: UnassignedWitnessScriptPubKey => false
    }
  }
}

object Bech32mAddress extends AddressFactory[Bech32mAddress] {

  private case class Bech32mAddressImpl(
      networkParameters: NetworkParameters,
      data: Vector[UInt5])
      extends Bech32mAddress {
    require(verifyChecksum, "checksum did not pass")
    require(Try(scriptPubKey).isSuccess,
            s"invalid witness script pub key, ${Try(scriptPubKey)}")
  }

  def empty(network: NetworkParameters = MainNet): Bech32mAddress =
    fromScriptPubKey(P2WSHWitnessSPKV0(EmptyScriptPubKey), network)

  def apply(
      witSPK: WitnessScriptPubKey,
      networkParameters: NetworkParameters): Bech32mAddress = {
    // we don't encode the wit version or pushop for program into base5
    val prog = UInt8.toUInt8s(witSPK.asmBytes.tail.tail)
    val encoded = Bech32.from8bitTo5bit(prog)
    val witVersion = witSPK.witnessVersion.version.toInt.toByte
    Bech32mAddress(networkParameters, Vector(UInt5(witVersion)) ++ encoded)
  }

  def apply(
      networkParameters: NetworkParameters,
      data: Vector[UInt5]): Bech32mAddress = {
    Bech32mAddressImpl(networkParameters, data)
  }

  /** Returns a base 5 checksum as specified by BIP173 */
  def createChecksum(
      hrp: BtcHumanReadablePart,
      bytes: Vector[UInt5]): Vector[UInt5] = {
    val values = hrp.expand ++ bytes
    Bech32.createChecksum(values, Bech32Encoding.Bech32m)
  }

  /** Tries to convert the given string a to a
    * [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]]
    */
  def fromStringToWitSPK(string: String): Try[WitnessScriptPubKey] = {
    val decoded = Bech32.splitToHrpAndData(string, Bech32m)
    decoded.flatMap { case (_, bytes) =>
      val v = bytes.head
      val convertedProg = NumberUtil.convertUInt5sToUInt8(bytes.tail)
      val progBytes = UInt8.toBytes(convertedProg)
      val witVersion = WitnessVersion(v.toInt)
      val pushOp = BitcoinScriptUtil.calculatePushOp(progBytes)
      witVersion match {
        case Some(v) =>
          val witSPK = Try(
            WitnessScriptPubKey.fromAsm(
              List(v.version) ++ pushOp ++ List(ScriptConstant(progBytes))))
          witSPK match {
            case Success(spk) => Success(spk)
            case Failure(err) =>
              Failure(
                new IllegalArgumentException(
                  "Failed to decode bech32 into a witSPK: " + err.getMessage))
          }
        case None =>
          Failure(
            new IllegalArgumentException(
              "Witness version was not valid, got: " + v))
      }
    }
  }

  /** Decodes bech32 string to the
    * [[org.bitcoins.core.protocol.BtcHumanReadablePart HumanReadablePart]] &
    * data part
    */
  override def fromString(bech32mStr: String): Bech32mAddress = {
    val bech32T = for {
      (hrp, data) <- Bech32.splitToHrpAndData(bech32mStr,
                                              Bech32Encoding.Bech32m)
      network = BtcHumanReadablePart.fromString(hrp).network
    } yield Bech32mAddress(network, data)

    bech32T match {
      case Success(bech32m) => bech32m
      case Failure(exn)     => throw exn
    }
  }

  override def fromScriptPubKeyT(
      spk: ScriptPubKey,
      np: NetworkParameters): Try[Bech32mAddress] =
    spk match {
      case x @ (_: NonWitnessScriptPubKey | _: WitnessScriptPubKeyV0 |
          _: UnassignedWitnessScriptPubKey) =>
        Failure(
          new IllegalArgumentException(
            "Cannot create a address for the scriptPubKey: " + x))
      case t: TaprootScriptPubKey =>
        Success(Bech32mAddress(t, np))
    }
}

object P2PKHAddress extends AddressFactory[P2PKHAddress] {

  private case class P2PKHAddressImpl(
      hash: Sha256Hash160Digest,
      networkParameters: NetworkParameters)
      extends P2PKHAddress

  def apply(
      hash: Sha256Hash160Digest,
      network: NetworkParameters): P2PKHAddress =
    P2PKHAddressImpl(hash, network)

  def apply(
      pubKey: ECPublicKey,
      networkParameters: NetworkParameters): P2PKHAddress = {
    val hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    P2PKHAddress(hash, networkParameters)
  }

  def apply(
      spk: P2PKHScriptPubKey,
      networkParameters: NetworkParameters): P2PKHAddress = {
    P2PKHAddress(spk.pubKeyHash, networkParameters)
  }

  def fromDecompressedPubKey(
      pubKey: ECPublicKey,
      networkParameters: NetworkParameters): P2PKHAddress = {
    val hash = CryptoUtil.sha256Hash160(pubKey.decompressedBytes)
    P2PKHAddressImpl(hash, networkParameters)
  }

  override def fromString(address: String): P2PKHAddress = {
    val decodeCheckP2PKH: Try[ByteVector] = Base58.decodeCheck(address)
    val p2pkhT = decodeCheckP2PKH.flatMap { bytes =>
      val networkBytes: Option[(NetworkParameters, ByteVector)] =
        Networks.knownNetworks
          .map(n => (n, n.p2pkhNetworkByte))
          .find { case (_, bs) =>
            bytes.startsWith(bs)
          }
      val result: Option[P2PKHAddress] = networkBytes.map {
        case (network, p2pkhNetworkBytes) =>
          val payloadSize = bytes.size - p2pkhNetworkBytes.size
          require(
            payloadSize == 20,
            s"Payload of a P2PKH address must be 20 bytes in size, got $payloadSize")
          val payload = bytes.slice(p2pkhNetworkBytes.size, bytes.size)
          P2PKHAddress(Sha256Hash160Digest(payload), network)
      }
      result match {
        case Some(addr) => Success(addr)
        case None =>
          Failure(
            new IllegalArgumentException(
              s"Given address was not a valid P2PKH address, got: $address"))
      }
    }

    p2pkhT match {
      case Success(p2pkh) => p2pkh
      case Failure(exn)   => throw exn
    }
  }

  override def fromScriptPubKeyT(
      spk: ScriptPubKey,
      np: NetworkParameters): Try[P2PKHAddress] =
    spk match {
      case p2pkh: P2PKHScriptPubKey => Success(P2PKHAddress(p2pkh, np))
      case x @ (_: P2PKScriptPubKey | _: P2PKWithTimeoutScriptPubKey |
          _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey |
          _: LockTimeScriptPubKey | _: ConditionalScriptPubKey |
          _: WitnessScriptPubKey | _: NonStandardScriptPubKey |
          _: WitnessCommitment | _: UnassignedWitnessScriptPubKey |
          EmptyScriptPubKey) =>
        Failure(
          new IllegalArgumentException(
            "Cannot create a address for the scriptPubKey: " + x))
    }
}

object P2SHAddress extends AddressFactory[P2SHAddress] {

  private case class P2SHAddressImpl(
      hash: Sha256Hash160Digest,
      networkParameters: NetworkParameters)
      extends P2SHAddress

  /** Creates a
    * [[org.bitcoins.core.protocol.script.P2SHScriptPubKey P2SHScriptPubKey]]
    * from the given
    * [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]], then
    * creates an address from that
    * [[org.bitcoins.core.protocol.script.P2SHScriptPubKey P2SHScriptPubKey]]
    */
  def apply(
      scriptPubKey: ScriptPubKey,
      network: NetworkParameters): P2SHAddress = {
    val p2shScriptPubKey = P2SHScriptPubKey(scriptPubKey)
    P2SHAddress(p2shScriptPubKey, network)
  }

  def apply(
      p2shScriptPubKey: P2SHScriptPubKey,
      network: NetworkParameters): P2SHAddress =
    P2SHAddress(p2shScriptPubKey.scriptHash, network)

  def apply(
      hash: Sha256Hash160Digest,
      network: NetworkParameters): P2SHAddress =
    P2SHAddressImpl(hash, network)

  override def fromString(address: String): P2SHAddress = {
    val decodeCheckP2SH: Try[ByteVector] = Base58.decodeCheck(address)
    val p2shT = decodeCheckP2SH.flatMap { bytes =>
      val networkBytes: Option[(NetworkParameters, ByteVector)] =
        Networks.knownNetworks
          .map(n => (n, n.p2shNetworkByte))
          .find { case (_, bs) =>
            bytes.startsWith(bs)
          }
      val result: Option[P2SHAddress] = networkBytes.map {
        case (network, p2shNetworkBytes) =>
          val payloadSize = bytes.size - p2shNetworkBytes.size
          require(
            payloadSize == 20,
            s"Payload of a P2PKH address must be 20 bytes in size, got $payloadSize")
          val payload = bytes.slice(p2shNetworkBytes.size, bytes.size)
          P2SHAddress(Sha256Hash160Digest(payload), network)
      }
      result match {
        case Some(addr) => Success(addr)
        case None =>
          Failure(
            new IllegalArgumentException(
              s"Given address was not a valid P2PKH address, got: $address"))
      }
    }

    p2shT match {
      case Success(p2sh) => p2sh
      case Failure(exn)  => throw exn
    }
  }

  override def fromScriptPubKeyT(
      spk: ScriptPubKey,
      np: NetworkParameters): Try[P2SHAddress] =
    spk match {
      case p2sh: P2SHScriptPubKey => Success(P2SHAddress(p2sh, np))
      case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
          _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
          _: LockTimeScriptPubKey | _: ConditionalScriptPubKey |
          _: WitnessScriptPubKey | _: NonStandardScriptPubKey |
          _: WitnessCommitment | _: UnassignedWitnessScriptPubKey |
          EmptyScriptPubKey) =>
        Failure(
          new IllegalArgumentException(
            "Cannot create a address for the scriptPubKey: " + x))
    }
}

object BitcoinAddress extends AddressFactory[BitcoinAddress] {

  /** Creates a [[org.bitcoins.core.protocol.BitcoinAddress BitcoinAddress]]
    * from the given string value
    */
  def apply(value: String): BitcoinAddress = fromString(value)

  override def fromString(value: String): BitcoinAddress = {
    val addressT = P2PKHAddress
      .fromStringT(value)
      .orElse(P2SHAddress.fromStringT(value))
      .orElse(Bech32Address.fromStringT(value))
      .orElse(Bech32mAddress.fromStringT(value))

    addressT match {
      case Success(addr) => addr
      case Failure(_) =>
        throw new IllegalArgumentException(
          s"Could not decode the given value to a BitcoinAddress, got: $value")
    }
  }

  override def fromScriptPubKeyT(
      spk: ScriptPubKey,
      np: NetworkParameters): Try[BitcoinAddress] =
    spk match {
      case p2pkh: P2PKHScriptPubKey      => Success(P2PKHAddress(p2pkh, np))
      case p2sh: P2SHScriptPubKey        => Success(P2SHAddress(p2sh, np))
      case witSPK: WitnessScriptPubKeyV0 => Success(Bech32Address(witSPK, np))
      case taprootSPK: TaprootScriptPubKey =>
        Success(Bech32mAddress(taprootSPK, np))
      case u: UnassignedWitnessScriptPubKey =>
        Success(Bech32mAddress(u, np))
      case x @ (_: P2PKScriptPubKey | _: P2PKWithTimeoutScriptPubKey |
          _: MultiSignatureScriptPubKey | _: LockTimeScriptPubKey |
          _: ConditionalScriptPubKey | _: NonStandardScriptPubKey |
          _: WitnessCommitment | EmptyScriptPubKey) =>
        Failure(
          new IllegalArgumentException(
            "Cannot create a address for the scriptPubKey: " + x))
    }
}

object Address extends AddressFactory[Address] {

  def fromBytes(bytes: ByteVector): Address = {
    val encoded = Base58.encode(bytes)
    BitcoinAddress.fromString(encoded)
  }

  def fromHex(hex: String): Address =
    fromBytes(BytesUtil.decodeHex(hex))

  def apply(bytes: ByteVector): Address = fromBytes(bytes)

  def apply(str: String): Address = fromString(str)

  override def fromString(str: String): Address = {
    BitcoinAddress.fromString(str)
  }

  override def fromScriptPubKeyT(
      spk: ScriptPubKey,
      network: NetworkParameters): Try[Address] =
    network match {
      case _: BitcoinNetwork => BitcoinAddress.fromScriptPubKeyT(spk, network)
    }

  def apply(
      spk: ScriptPubKey,
      networkParameters: NetworkParameters): Try[Address] = {
    fromScriptPubKeyT(spk, networkParameters)
  }
}
