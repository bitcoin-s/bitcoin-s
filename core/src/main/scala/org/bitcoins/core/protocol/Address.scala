package org.bitcoins.core.protocol
import org.bitcoins.core.config._
import org.bitcoins.core.config.{ MainNet, RegTest, TestNet3 }
import org.bitcoins.core.crypto._
import org.bitcoins.core.number.{ UInt32, UInt5, UInt8 }
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.script.constant.ScriptConstant
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.util._
import org.slf4j.LoggerFactory
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.util.{ Failure, Success, Try }

sealed abstract class Address {

  /** The network that this address is valid for */
  def networkParameters: NetworkParameters

  /** The string representation of this address */
  def value: String

  /** Every address is derived from a [[HashDigest]] in a [[TransactionOutput]] */
  def hash: HashDigest

  /** The [[ScriptPubKey]] the address represents */
  def scriptPubKey: ScriptPubKey

  override def toString = value
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

}

sealed abstract class P2SHAddress extends BitcoinAddress {
  /** The base58 string representation of this address */
  override def value: String = {
    val versionByte = networkParameters.p2shNetworkByte
    val bytes = versionByte ++ hash.bytes
    val checksum = CryptoUtil.doubleSHA256(bytes).bytes.take(4)
    Base58.encode(bytes ++ checksum)
  }

  override def scriptPubKey = P2SHScriptPubKey(hash)

  override def hash: Sha256Hash160Digest
}

/**
 * https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki
 */
sealed abstract class Bech32Address extends BitcoinAddress {

  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  def hrp: HumanReadablePart

  def data: Vector[UInt5]

  override def networkParameters = hrp.network.get

  override def value: String = {
    val all: Vector[UInt5] = data ++ checksum
    val encoding = Bech32.encode5bitToString(all)

    hrp.toString + Bech32.separator + encoding
  }

  def checksum: Vector[UInt5] = Bech32Address.createChecksum(hrp, data)

  override def scriptPubKey: WitnessScriptPubKey = {
    Bech32Address.fromStringToWitSPK(value).get
  }

  override def hash: HashDigest = {
    val byteVector = BitcoinSUtil.toByteVector(scriptPubKey.witnessProgram)
    scriptPubKey match {
      case _: P2WPKHWitnessSPKV0 =>
        RipeMd160Digest(byteVector)
      case _: P2WSHWitnessSPKV0 =>
        Sha256Digest(byteVector)
      case _: UnassignedWitnessScriptPubKey =>
        throw new IllegalArgumentException(s"Cannot parse the hash of an unassigned witness scriptpubkey for bech32 address")
    }
  }

  override def toString = "Bech32Address(" + value + ")"

}

object Bech32Address extends AddressFactory[Bech32Address] {
  private case class Bech32AddressImpl(hrp: HumanReadablePart, data: Vector[UInt5]) extends Bech32Address {
    //require(verifyChecksum(hrp, data), "checksum did not pass")
  }

  private val logger = BitcoinSLogger.logger

  def apply(
    witSPK: WitnessScriptPubKey,
    networkParameters: NetworkParameters): Try[Bech32Address] = {
    //we don't encode the wit version or pushop for program into base5
    val prog = UInt8.toUInt8s(witSPK.asmBytes.tail.tail)
    val encoded = Bech32.from8bitTo5bit(prog)
    val hrp = networkParameters match {
      case _: MainNet => bc
      case _: TestNet3 | _: RegTest => tb
    }
    val witVersion = witSPK.witnessVersion.version.toInt.toByte
    Try(Bech32Address(hrp, Vector(UInt5(witVersion)) ++ encoded))
  }

  def apply(hrp: HumanReadablePart, data: Vector[UInt5]): Bech32Address = {
    Bech32AddressImpl(hrp, data)
  }

  /** Returns a base 5 checksum as specified by BIP173 */
  def createChecksum(hrp: HumanReadablePart, bytes: Vector[UInt5]): Vector[UInt5] = {
    val values = hrpExpand(hrp) ++ bytes
    Bech32.createChecksum(values)
  }

  def hrpExpand(hrp: HumanReadablePart): Vector[UInt5] = {
    Bech32.hrpExpand(hrp.bytes)
  }

  def verifyChecksum(hrp: HumanReadablePart, u5s: Seq[UInt5]): Boolean = {
    val data = hrpExpand(hrp) ++ u5s
    val checksum = Bech32.polyMod(data)
    checksum == 1
  }

  private val u32Five = UInt32(5)
  private val u32Eight = UInt32(8)

  /** Tries to convert the given string a to a [[org.bitcoins.core.protocol.script.WitnessScriptPubKey]] */
  def fromStringToWitSPK(string: String): Try[WitnessScriptPubKey] = {
    val decoded = fromString(string)
    decoded.flatMap {
      case bech32Addr =>
        val bytes = bech32Addr.data
        val (v, prog) = (bytes.head, bytes.tail)
        val convertedProg = NumberUtil.convertUInt5sToUInt8(bytes.tail)
        val progBytes = UInt8.toBytes(convertedProg)
        val witVersion = WitnessVersion(v.toInt)
        val pushOp = BitcoinScriptUtil.calculatePushOp(progBytes)
        witVersion match {
          case Some(v) =>
            WitnessScriptPubKey(List(v.version) ++ pushOp ++ List(ScriptConstant(progBytes))) match {
              case Some(spk) => Success(spk)
              case None => Failure(new IllegalArgumentException("Failed to decode bech32 into a witSPK"))
            }
          case None => Failure(new IllegalArgumentException("Witness version was not valid, got: " + v))
        }

    }
  }

  /** Decodes bech32 string to the [[HumanReadablePart]] & data part */
  override def fromString(str: String): Try[Bech32Address] = {
    val sepIndexes = str.zipWithIndex.filter(_._1 == Bech32.separator)
    if (str.size > 90 || str.size < 8) {
      Failure(new IllegalArgumentException("bech32 payloads must be betwee 8 and 90 chars, got: " + str.size))
    } else if (sepIndexes.isEmpty) {
      Failure(new IllegalArgumentException("Bech32 address did not have the correct separator"))
    } else {
      val sepIndex = sepIndexes.last._2
      val (hrp, data) = (str.take(sepIndex), str.splitAt(sepIndex + 1)._2)
      if (hrp.size < 1 || data.size < 6) {
        Failure(new IllegalArgumentException("Hrp/data too short"))
      } else {
        val hrpValid = checkHrpValidity(hrp)
        val dataValid = Bech32.checkDataValidity(data)
        val isChecksumValid: Try[Vector[UInt5]] = hrpValid.flatMap { h: HumanReadablePart =>
          dataValid.flatMap { d: Vector[UInt5] =>
            if (verifyChecksum(h, d)) {
              if (d.size < 6) Success(Vector.empty)
              else Success(d.take(d.size - 6))
            } else Failure(new IllegalArgumentException("Checksum was invalid on the bech32 address"))
          }
        }

        isChecksumValid.flatMap { d: Vector[UInt5] =>
          hrpValid.map(h => Bech32Address(h, d))
        }
      }
    }
  }

  override def fromScriptPubKey(spk: ScriptPubKey, np: NetworkParameters): Try[Bech32Address] = spk match {
    case witSPK: WitnessScriptPubKey => Bech32Address.fromScriptPubKey(witSPK, np)
    case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey
      | _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey
      | _: LockTimeScriptPubKey | _: WitnessScriptPubKey
      | _: EscrowTimeoutScriptPubKey | _: NonStandardScriptPubKey
      | _: WitnessCommitment | _: UnassignedWitnessScriptPubKey | EmptyScriptPubKey) =>
      Failure(new IllegalArgumentException("Cannot create a address for the scriptPubKey: " + x))
  }

  /** Checks if the possible human readable part follows BIP173 rules */
  private def checkHrpValidity(hrp: String): Try[HumanReadablePart] = {
    @tailrec
    def loop(remaining: List[Char], accum: Seq[UInt8], isLower: Boolean, isUpper: Boolean): Try[Seq[UInt8]] = remaining match {
      case h :: t =>
        if (h < 33 || h > 126) {
          Failure(new IllegalArgumentException("Invalid character range for hrp, got: " + hrp))
        } else if (isLower && isUpper) {
          Failure(new IllegalArgumentException("HRP had mixed case, got: " + hrp))
        } else {
          loop(t, UInt8(h.toByte) +: accum, h.isLower || isLower, h.isUpper || isUpper)
        }
      case Nil =>
        if (isLower && isUpper) {
          Failure(new IllegalArgumentException("HRP had mixed case, got: " + hrp))
        } else {
          Success(accum.reverse)
        }
    }

    loop(hrp.toCharArray.toList, Nil, false, false).flatMap { _ =>
      Success(HumanReadablePart(hrp.toLowerCase))
    }
  }

}

object P2PKHAddress extends AddressFactory[P2PKHAddress] {
  private case class P2PKHAddressImpl(
    hash: Sha256Hash160Digest,
    networkParameters: NetworkParameters) extends P2PKHAddress

  def apply(hash: Sha256Hash160Digest, network: NetworkParameters): P2PKHAddress = P2PKHAddressImpl(hash, network)

  def apply(pubKey: ECPublicKey, networkParameters: NetworkParameters): P2PKHAddress = {
    val hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    P2PKHAddress(hash, networkParameters)
  }

  def apply(spk: P2PKHScriptPubKey, networkParameters: NetworkParameters): P2PKHAddress = {
    P2PKHAddress(spk.pubKeyHash, networkParameters)
  }

  override def fromString(address: String): Try[P2PKHAddress] = {
    val decodeCheckP2PKH: Try[ByteVector] = Base58.decodeCheck(address)
    decodeCheckP2PKH.flatMap { bytes =>
      val networkBytes: Option[(NetworkParameters, ByteVector)] = Networks.knownNetworks.map(n => (n, n.p2pkhNetworkByte))
        .find {
          case (_, bs) =>
            bytes.startsWith(bs)
        }
      val result: Option[P2PKHAddress] = networkBytes.map {
        case (network, p2pkhNetworkBytes) =>
          val payloadSize = bytes.size - p2pkhNetworkBytes.size
          require(payloadSize == 20, s"Payload of a P2PKH address must be 20 bytes in size, got $payloadSize")
          val payload = bytes.slice(p2pkhNetworkBytes.size, bytes.size)
          P2PKHAddress(Sha256Hash160Digest(payload), network)
      }
      result match {
        case Some(addr) => Success(addr)
        case None => Failure(new IllegalArgumentException(s"Given address was not a valid P2PKH address, got: $address"))
      }
    }
  }

  override def fromScriptPubKey(spk: ScriptPubKey, np: NetworkParameters): Try[P2PKHAddress] = spk match {
    case p2pkh: P2PKHScriptPubKey => Success(P2PKHAddress(p2pkh, np))
    case x @ (_: P2PKScriptPubKey | _: MultiSignatureScriptPubKey
      | _: P2SHScriptPubKey | _: LockTimeScriptPubKey
      | _: WitnessScriptPubKey | _: EscrowTimeoutScriptPubKey
      | _: NonStandardScriptPubKey | _: WitnessCommitment
      | _: UnassignedWitnessScriptPubKey | EmptyScriptPubKey) =>
      Failure(new IllegalArgumentException("Cannot create a address for the scriptPubKey: " + x))
  }
}

object P2SHAddress extends AddressFactory[P2SHAddress] {
  private case class P2SHAddressImpl(
    hash: Sha256Hash160Digest,
    networkParameters: NetworkParameters) extends P2SHAddress

  /**
   * Creates a [[P2SHScriptPubKey]] from the given [[ScriptPubKey]],
   * then creates an address from that [[P2SHScriptPubKey]]
   */
  def apply(scriptPubKey: ScriptPubKey, network: NetworkParameters): P2SHAddress = {
    val p2shScriptPubKey = P2SHScriptPubKey(scriptPubKey)
    P2SHAddress(p2shScriptPubKey, network)
  }

  def apply(p2shScriptPubKey: P2SHScriptPubKey, network: NetworkParameters): P2SHAddress = P2SHAddress(p2shScriptPubKey.scriptHash, network)

  def apply(hash: Sha256Hash160Digest, network: NetworkParameters): P2SHAddress = P2SHAddressImpl(hash, network)

  override def fromString(address: String): Try[P2SHAddress] = {
    val decodeCheckP2SH: Try[ByteVector] = Base58.decodeCheck(address)
    decodeCheckP2SH.flatMap { bytes =>
      val networkBytes: Option[(NetworkParameters, ByteVector)] = Networks.knownNetworks.map(n => (n, n.p2shNetworkByte))
        .find {
          case (_, bs) =>
            bytes.startsWith(bs)
        }
      val result: Option[P2SHAddress] = networkBytes.map {
        case (network, p2shNetworkBytes) =>
          val payloadSize = bytes.size - p2shNetworkBytes.size
          require(payloadSize == 20, s"Payload of a P2PKH address must be 20 bytes in size, got $payloadSize")
          val payload = bytes.slice(p2shNetworkBytes.size, bytes.size)
          P2SHAddress(Sha256Hash160Digest(payload), network)
      }
      result match {
        case Some(addr) => Success(addr)
        case None => Failure(new IllegalArgumentException(s"Given address was not a valid P2PKH address, got: $address"))
      }
    }
  }

  override def fromScriptPubKey(spk: ScriptPubKey, np: NetworkParameters): Try[P2SHAddress] = spk match {
    case p2sh: P2SHScriptPubKey => Success(P2SHAddress(p2sh, np))
    case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey
      | _: LockTimeScriptPubKey | _: WitnessScriptPubKey
      | _: EscrowTimeoutScriptPubKey | _: NonStandardScriptPubKey
      | _: WitnessCommitment | _: UnassignedWitnessScriptPubKey | EmptyScriptPubKey) =>
      Failure(new IllegalArgumentException("Cannot create a address for the scriptPubKey: " + x))
  }
}

object BitcoinAddress extends AddressFactory[BitcoinAddress] {
  private val logger = BitcoinSLogger.logger

  /** Creates a [[BitcoinAddress]] from the given string value */
  def apply(value: String): Try[BitcoinAddress] = fromString(value)

  override def fromString(value: String): Try[BitcoinAddress] = {
    val p2pkhTry = P2PKHAddress.fromString(value)
    if (p2pkhTry.isSuccess) {
      p2pkhTry
    } else {
      val p2shTry = P2SHAddress.fromString(value)
      if (p2shTry.isSuccess) {
        p2shTry
      } else {
        val bech32Try = Bech32Address.fromString(value)
        if (bech32Try.isSuccess) {
          bech32Try
        } else {
          Failure(new IllegalArgumentException(s"Could not decode the given value to a BitcoinAddress, got: $value"))
        }
      }
    }
  }

  override def fromScriptPubKey(spk: ScriptPubKey, np: NetworkParameters): Try[BitcoinAddress] = spk match {
    case p2pkh: P2PKHScriptPubKey => Success(P2PKHAddress(p2pkh, np))
    case p2sh: P2SHScriptPubKey => Success(P2SHAddress(p2sh, np))
    case witSPK: WitnessScriptPubKey => Bech32Address(witSPK, np)
    case x @ (_: P2PKScriptPubKey | _: MultiSignatureScriptPubKey | _: LockTimeScriptPubKey
      | _: EscrowTimeoutScriptPubKey | _: NonStandardScriptPubKey
      | _: WitnessCommitment | _: UnassignedWitnessScriptPubKey | EmptyScriptPubKey) =>
      Failure(new IllegalArgumentException("Cannot create a address for the scriptPubKey: " + x))
  }

}

object Address extends AddressFactory[Address] {

  def fromBytes(bytes: ByteVector): Try[Address] = {
    val encoded = Base58.encode(bytes)
    BitcoinAddress.fromString(encoded)
  }

  def fromHex(hex: String): Try[Address] = fromBytes(BitcoinSUtil.decodeHex(hex))

  def apply(bytes: ByteVector): Try[Address] = fromBytes(bytes)

  def apply(str: String): Try[Address] = fromString(str)

  override def fromString(str: String): Try[Address] = {
    BitcoinAddress.fromString(str)
  }
  override def fromScriptPubKey(spk: ScriptPubKey, network: NetworkParameters): Try[Address] = network match {
    case bitcoinNetwork: BitcoinNetwork => BitcoinAddress.fromScriptPubKey(spk, network)
  }
  def apply(spk: ScriptPubKey, networkParameters: NetworkParameters): Try[Address] = {
    fromScriptPubKey(spk, networkParameters)
  }
}
