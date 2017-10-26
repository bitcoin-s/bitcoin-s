package org.bitcoins.core.protocol
import org.bitcoins.core.config._
import org.bitcoins.core.config.{MainNet, RegTest, TestNet3}
import org.bitcoins.core.crypto.{ECPublicKey, HashDigest, Sha256Digest, Sha256Hash160Digest}
import org.bitcoins.core.number.{UInt32, UInt8}
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.script.constant.ScriptConstant
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.util._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

sealed abstract class Address {

  /** The network that this address is valid for */
  def networkParameters: NetworkParameters

  /** The string representation of this address */
  def value : String

  /** Every address is derived from a [[Sha256Hash160Digest]] in a [[TransactionOutput]] */
  def hash: HashDigest

  /** The [[ScriptPubKey]] the address represents */
  def scriptPubKey: ScriptPubKey
}

sealed abstract class BitcoinAddress extends Address

sealed abstract class P2PKHAddress extends BitcoinAddress {
  /** The base58 string representation of this address */
  override def value : String = {
    val versionByte = networkParameters.p2pkhNetworkByte
    val bytes = Seq(versionByte) ++ hash.bytes
    val checksum = CryptoUtil.doubleSHA256(bytes).bytes.take(4)
    Base58.encode(bytes ++ checksum)
  }

  override def hash: Sha256Hash160Digest

  override def scriptPubKey: P2PKHScriptPubKey = P2PKHScriptPubKey(hash)

}

object P2PKHAddress {
  private case class P2PKHAddressImpl(hash: Sha256Hash160Digest,
                                      networkParameters: NetworkParameters) extends P2PKHAddress {
    require(isP2PKHAddress(value), "Bitcoin address was invalid " + value)
  }

  /**
    * Encodes a pubkey hash to a base 58 address on the corresponding network
    *
    * @param hash the result of Sha256(RipeMD160(pubkey))
    * @param network the network on which this address is being generated for
    * @return
    */
  def encodePubKeyHashToAddress(hash: Sha256Hash160Digest, network: NetworkParameters): P2PKHAddress = P2PKHAddressImpl(hash,network)

  def apply(hash: Sha256Hash160Digest, networkParameters: NetworkParameters): P2PKHAddress = encodePubKeyHashToAddress(hash,networkParameters)

  def apply(pubKey: ECPublicKey, networkParameters: NetworkParameters): P2PKHAddress = {
    val hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    P2PKHAddress(hash,networkParameters)
  }

  def apply(spk: P2PKHScriptPubKey, networkParameters: NetworkParameters): P2PKHAddress = {
    P2PKHAddress(spk.pubKeyHash,networkParameters)
  }

  /**
    * Checks if an address is a valid p2pkh address
    *
    * @param address
    * @return
    */
  def isP2PKHAddress(address : String) : Boolean = {
    val decodeCheckP2PKH : Try[Seq[Byte]] = Base58.decodeCheck(address)
    decodeCheckP2PKH match {
      case Success(bytes) =>
        val firstByte = bytes.head
        (firstByte == MainNet.p2pkhNetworkByte || firstByte == TestNet3.p2pkhNetworkByte ||
          firstByte == RegTest.p2pkhNetworkByte) && bytes.size == 21
      case Failure(exception) => false
    }
  }

  /**
    * Checks if an address is a valid p2pkh address
    *
    * @param address
    * @return
    */
  def isP2PKHAddress(address : BitcoinAddress) : Boolean = isP2PKHAddress(address.value)

}

sealed abstract class P2SHAddress extends BitcoinAddress {
  /** The base58 string representation of this address */
  override def value : String = {
    val versionByte = networkParameters.p2shNetworkByte
    val bytes = Seq(versionByte) ++ hash.bytes
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

  private def logger = BitcoinSLogger.logger

  def hrp: HumanReadablePart

  def data: Seq[UInt8]

  override def networkParameters = hrp.network

  override def value: String = {
    val checksum = Bech32Address.createChecksum(hrp,data)
    val all = data ++ checksum
    val encoding = Bech32Address.encodeToString(all)
    hrp.toString + "1" + encoding
  }

  override def scriptPubKey: WitnessScriptPubKey = {
    Bech32Address.fromString(value).get
  }

  override def hash: Sha256Digest = Sha256Digest(scriptPubKey.witnessProgram.flatMap(_.bytes))

}

object Bech32Address {
  private case class Bech32AddressImpl(hrp: HumanReadablePart, data: Seq[UInt8]) extends Bech32Address

  private val logger = BitcoinSLogger.logger

  def isValid(bytes: Seq[Byte]): Boolean = ???

  def apply(witSPK: WitnessScriptPubKey,
            networkParameters: NetworkParameters): Try[Bech32Address] = {
    //we don't encode the wit version or pushop for program into base5
    val prog = UInt8.toUInt8s(witSPK.asmBytes.tail.tail)
    val encoded = Bech32Address.encode(prog)
    val hrp = networkParameters match {
      case _: MainNet => bc
      case _: TestNet3 | _: RegTest => tb
    }
    //add witversion
    encoded.map(e => Bech32Address(hrp,Seq(UInt8.zero) ++ e))
  }


  def apply(hrp: HumanReadablePart, data: Seq[UInt8]): Bech32Address = {
    Bech32AddressImpl(hrp,data)
  }

  /** Returns a base 5 checksum as specified by BIP173 */
  def createChecksum(hrp: HumanReadablePart, bytes: Seq[UInt8]): Seq[UInt8] = {
    val values: Seq[UInt8] = hrpExpand(hrp) ++ bytes
    val z = UInt8.zero
    val polymod: Long = polyMod(values ++ Seq(z,z,z,z,z,z)) ^ 1
    //[(polymod >> 5 * (5 - i)) & 31 for i in range(6)]
    val result: Seq[UInt8] = 0.until(6).map { i =>
      val u = UInt8(i.toShort)
      val five = UInt8(5.toShort)
      //((polymod >> five * (five - u)) & UInt8(31.toShort))
      UInt8(((polymod >> 5 * (5 - i)) & 31).toShort)
    }
    result
  }

  def hrpExpand(hrp: HumanReadablePart): Seq[UInt8] = {
    val x: Seq[Byte] = hrp.bytes.map { b: Byte =>
      (b >> 5).toByte
    }
    val withZero: Seq[Byte] = x ++ Seq(0.toByte)

    val y: Seq[Byte] = hrp.bytes.map { char =>
      (char & 0x1f).toByte
    }
    val result = UInt8.toUInt8s(withZero ++ y)
    result
  }

  private def generators: Seq[Long] = Seq(UInt32("3b6a57b2").toLong,
    UInt32("26508e6d").toLong, UInt32("1ea119fa").toLong,
    UInt32("3d4233dd").toLong, UInt32("2a1462b3").toLong)

  def polyMod(bytes: Seq[UInt8]): Long = {
    var chk: Long = 1
    bytes.map { v =>
      val b = chk >> 25
      //chk = (chk & 0x1ffffff) << 5 ^ v
      chk = (chk & 0x1ffffff) << 5 ^ v.underlying
      0.until(5).map { i: Int =>
        //chk ^= GEN[i] if ((b >> i) & 1) else 0
        if (((b >> i) & 1) == 1) {
          chk = chk ^ generators(i)
        }
      }
    }
    chk
  }

  def verifyChecksum(hrp: HumanReadablePart, data: Seq[Byte]): Boolean = {
    val u8s = UInt8.toUInt8s(data)
    polyMod(hrpExpand(hrp) ++ u8s) == 1
  }

  private val u32Five = UInt32(5)
  private val u32Eight = UInt32(8)
  /** The separator between the hrp and payload of bech 32 */
  private val separator = '1'
  /** Converts a byte array from base 8 to base 5 */
  def encode(bytes: Seq[UInt8]): Try[Seq[UInt8]] = {
    NumberUtil.convertUInt8s(bytes,u32Eight,u32Five,true)
  }
  /** Decodes a byte array from base 5 to base 8 */
  def decode(b: Seq[UInt8]): Try[Seq[UInt8]] = {
    NumberUtil.convertUInt8s(b,u32Five,u32Eight,false)
  }

  /** Takes a base32 byte array and encodes it to a string */
  def encodeToString(b: Seq[UInt8]): String = {
    b.map(b => charset(b.underlying)).mkString
  }
  /** Decodes bech32 string to a spk */
  def fromString(str: String): Try[WitnessScriptPubKey] = {
    if (str.size > 90 || str.size < 8) {
      Failure(new IllegalArgumentException("bech32 payloads must be betwee 8 and 90 chars, got: " + str.size))
    } else if (str(2) != separator) {
      Failure(new IllegalArgumentException("Bech32 address did not have the correct separator, got: " + str(2)))
    } else {
      val (hrp,data) = (str.take(2), str.splitAt(3)._2)
      if (hrp.size < 1 || data.size < 6) {
        Failure(new IllegalArgumentException("Hrp/data too short"))
      } else {
        val hrpValid = checkHrpValidity(hrp)
        val dataValid = checkDataValidity(data)
        val isChecksumValid: Try[Seq[Byte]] = hrpValid.flatMap { h =>
          dataValid.flatMap { d =>
            if (verifyChecksum(h,d)) {
              //remove checksum bytes since it is valid
              Success(d.take(d.size - 6))
            }
            else Failure(new IllegalArgumentException("Checksum was invalid on the bech32 address"))
          }
        }
        isChecksumValid.flatMap { bytes: Seq[Byte] =>
          val (v,prog) = (bytes.head,bytes.tail)
          val convertedProg = NumberUtil.convertBytes(prog,u32Five,u32Eight,false)
          val progBytes = convertedProg.map(UInt8.toBytes(_))
          val witVersion = WitnessVersion(v)
          progBytes.flatMap { prog =>
            val pushOp = BitcoinScriptUtil.calculatePushOp(prog)
            WitnessScriptPubKey(Seq(witVersion.version) ++ pushOp ++ Seq(ScriptConstant(prog))) match {
              case Some(spk) => Success(spk)
              case None => Failure(new IllegalArgumentException("Failed to decode bech32 into a witSPK"))
            }
          }
        }
      }
    }
  }

  private def checkHrpValidity(hrp: String): Try[HumanReadablePart] = {
    var (isLower,isUpper) = (false,false)
    @tailrec
    def loop(remaining: List[Char], accum: Seq[UInt8]): Try[Seq[UInt8]] = remaining match {
      case h :: t =>
        if (h < 33 || h > 126) {
          Failure(new IllegalArgumentException("Invalid character range for hrp, got: " + hrp))
        } else {
          if (h.isLower) {
            isLower = true
          } else if (h.isUpper) {
            isUpper = true
          }
          loop(t,UInt8(h.toByte) +: accum)
        }
      case Nil =>
        Success(accum.reverse)
    }
    loop(hrp.toCharArray.toList,Nil).map { _ =>
      HumanReadablePart(hrp)
    }
  }

  def checkDataValidity(data: String): Try[Seq[Byte]] = {
    @tailrec
    def loop(remaining: List[Char], accum: Seq[Byte], hasUpper: Boolean, hasLower: Boolean): Try[Seq[Byte]] = remaining match {
      case Nil => Success(accum.reverse)
      case h :: t =>
        if (!charset.contains(h)) {
          Failure(new IllegalArgumentException("Invalid character in data of bech32 address, got: " + h))
        } else {
          if ((h.isUpper && hasLower) || (h.isLower && hasUpper)) {
            Failure(new IllegalArgumentException("Cannot have mixed case for bech32 address"))
          } else {
            val byte = charset.indexOf(h).toByte
            require(byte >= 0 && byte < 32)
            loop(t,  byte +: accum, h.isUpper || hasUpper, h.isLower || hasLower)
          }
        }
    }
    val payload: Try[Seq[Byte]] = loop(data.toCharArray.toList,Nil,false,false)
    payload
  }

  /** https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#bech32 */
  def charset: Seq[Char] = Seq('q', 'p', 'z', 'r', 'y', '9', 'x', '8',
    'g', 'f', '2', 't', 'v', 'd', 'w', '0',
    's', '3', 'j', 'n', '5', '4', 'k', 'h',
    'c', 'e', '6', 'm', 'u', 'a', '7', 'l')
}
/**
  * [[P2SHAddress]] companion object
  */
object P2SHAddress {
  private case class P2SHAddressImpl(hash: Sha256Hash160Digest,
                                     networkParameters: NetworkParameters) extends P2SHAddress {
    require(isP2SHAddress(value), "Bitcoin address was invalid " + value)
  }

  /**
    * Takes in an arbitrary [[ScriptPubKey]] and [[NetworkParameters]] and creates a [[P2SHAddress]]
    *
    * @param scriptPubKey the script which will need to provided as the redeem script
    * @param network the network which this address is valid for
    * @return the [[P2SHAddress]]
    */
  def encodeScriptPubKeyToAddress(scriptPubKey: ScriptPubKey, network: NetworkParameters): P2SHAddress = {
    val p2shScriptPubKey = P2SHScriptPubKey(scriptPubKey)
    P2SHAddress(p2shScriptPubKey,network)
  }


  /**
    * Creates a [[P2SHScriptPubKey]] from the given [[ScriptPubKey]], then creates an address from that [[P2SHScriptPubKey]]
    * @param scriptPubKey
    * @param network
    * @return
    */
  def apply(scriptPubKey: ScriptPubKey,network: NetworkParameters): P2SHAddress = encodeScriptPubKeyToAddress(scriptPubKey,network)


  def apply(p2shScriptPubKey: P2SHScriptPubKey, network: NetworkParameters): P2SHAddress = P2SHAddress(p2shScriptPubKey.scriptHash,network)


  def apply(hash: Sha256Hash160Digest, network: NetworkParameters): P2SHAddress = P2SHAddressImpl(hash, network)

  /**
    * Checks if a address is a valid p2sh address
    * @param address
    * @return
    */
  def isP2SHAddress(address : String) : Boolean = {
    val decodeCheckP2SH : Try[Seq[Byte]] = Base58.decodeCheck(address)
    decodeCheckP2SH match {
      case Success(bytes) =>
        val firstByte = bytes.head
        ((firstByte == MainNet.p2shNetworkByte || firstByte == TestNet3.p2shNetworkByte ||
          RegTest.p2shNetworkByte == firstByte)
          && bytes.size == 21)
      case Failure(exception) => false
    }
  }

  /**
    * Checks if a address is a valid p2sh address
    *
    * @param address
    * @return
    */
  def isP2SHAddress(address : BitcoinAddress) : Boolean = isP2SHAddress(address.value)

}

object BitcoinAddress {
  /** Checks if the given base58 bitcoin address is a valid address */
  def validate(bitcoinAddress: String): Boolean = {
    val decodeChecked = Base58.decodeCheck(bitcoinAddress)
    decodeChecked.isSuccess
  }


  /** Creates a [[BitcoinAddress]] from the given base58 string value */
  def apply(value: String): BitcoinAddress = {
    val decodeChecked = Base58.decodeCheck(value)
    decodeChecked match {
      case Success(bytes) =>
        val network = matchNetwork(bytes.head)
        if (P2PKHAddress.isP2PKHAddress(value)) {
          P2PKHAddress(Sha256Hash160Digest(bytes.tail),network)
        }
        else if (P2SHAddress.isP2SHAddress(value)) {
          P2SHAddress(Sha256Hash160Digest(bytes.tail), network)
        } else throw new IllegalArgumentException("The address was not a p2pkh or p2sh address, got: " + value)
      case Failure(exception) =>
        throw exception
    }
  }

  /** Helper function for helping matching an address to a network byte */
  private def matchNetwork(byte: Byte): NetworkParameters = byte match {
    case _ if Seq(MainNet.p2pkhNetworkByte,MainNet.p2shNetworkByte).contains(byte) => MainNet
    case _ if Seq(TestNet3.p2pkhNetworkByte, TestNet3.p2shNetworkByte).contains(byte) => TestNet3
    case _ if Seq(RegTest.p2pkhNetworkByte,RegTest.p2shNetworkByte).contains(byte) => RegTest
  }
}

object Address extends Factory[Address] {
  /**
    * Factory method for creating addresses
    * Takes in a string to check if it is an address
    * if it is it creates the address
    * if not it throws a runtime exception
    *
    * @param str
    * @return
    */
  def factory(str : String) : Address = {
    if (BitcoinAddress.validate(str)) BitcoinAddress(str)
    else throw new RuntimeException("The address that you passed in is invalid")
  }

  def fromBytes(bytes : Seq[Byte]) : Address = factory(Base58.encode(bytes))

  override def fromHex(hex : String) : Address = throw new RuntimeException("We cannot create a bitcoin address from hex - bitcoin addresses are base 58 encoded")

  override def apply(str : String) : Address = factory(str)

  def fromScriptPubKey(spk: ScriptPubKey, network: NetworkParameters): Try[BitcoinAddress] = spk match {
    case p2pkh: P2PKHScriptPubKey => Success(P2PKHAddress(p2pkh,network))
    case p2sh: P2SHScriptPubKey => Success(P2SHAddress(p2sh,network))
    case witSPK: WitnessScriptPubKeyV0 => Bech32Address(witSPK,network)
    case x @ (_: P2PKScriptPubKey | _: MultiSignatureScriptPubKey | _: LockTimeScriptPubKey
              | _: EscrowTimeoutScriptPubKey | _: NonStandardScriptPubKey
              | _: WitnessCommitment |  _: UnassignedWitnessScriptPubKey | EmptyScriptPubKey) =>
      Failure(new IllegalArgumentException("Cannot create a address for the scriptPubKey: " + x))
  }

  def apply(spk: ScriptPubKey, networkParameters: NetworkParameters): Try[BitcoinAddress] = {
    fromScriptPubKey(spk,networkParameters)
  }
}