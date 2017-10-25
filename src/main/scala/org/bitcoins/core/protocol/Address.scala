package org.bitcoins.core.protocol
import org.bitcoins.core.config._
import org.bitcoins.core.config.{MainNet, RegTest, TestNet3}
import org.bitcoins.core.crypto.{ECPublicKey, Sha256Hash160Digest}
import org.bitcoins.core.number.{UInt32, UInt8}
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.util._

import scala.util.{Failure, Success, Try}

sealed abstract class Address {

  /** The network that this address is valid for */
  def networkParameters: NetworkParameters

  /** The string representation of this address */
  def value : String

  /** Every address is derived from a [[Sha256Hash160Digest]] in a [[TransactionOutput]] */
  def hash: Sha256Hash160Digest

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

  override def scriptPubKey: ScriptPubKey = ???

  override def hash: Sha256Hash160Digest = ???

}

object Bech32Address {
  private case class Bech32AddressImpl(hrp: HumanReadablePart, data: Seq[UInt8]) extends Bech32Address

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

  def verifyChecksum(hrp: HumanReadablePart, data: Seq[UInt8]): Boolean = {
    polyMod(hrpExpand(hrp) ++ data) == 1
  }

  private val u32Five = UInt32(5)
  private val u32Eight = UInt32(8)
  /** Converts a byte array from base 8 to base 5 */
  def encode(bytes: Seq[UInt8]): Try[Seq[UInt8]] = {
    NumberUtil.convertBits(bytes,u32Eight,u32Five,true)
  }
  /** Decodes a byte array from base 5 to base 8 */
  def decode(b: Seq[UInt8]): Try[Seq[UInt8]] = {
    NumberUtil.convertBits(b,u32Five,u32Eight,false)
  }

  /** Takes a base32 byte array and encodes it to a string */
  def encodeToString(b: Seq[UInt8]): String = {
    b.map(b => charset(b.underlying)).mkString
  }
  /** Decodes a base32 string to a base 32 byte array */
  def decodeFromString(string: String): Try[Seq[Byte]] = {
    val invariant = Try(require(string.exists(charset.contains(_)), "String contained a non base32 character"))
    ???
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
    case x @ (_: P2PKScriptPubKey | _: MultiSignatureScriptPubKey | _: LockTimeScriptPubKey
              | _: EscrowTimeoutScriptPubKey | _: NonStandardScriptPubKey | _: WitnessScriptPubKeyV0
              | _: WitnessCommitment |  _: UnassignedWitnessScriptPubKey | EmptyScriptPubKey) =>
      Failure(new IllegalArgumentException("Cannot create a address for the scriptPubKey: " + x))
  }

  def apply(spk: ScriptPubKey, networkParameters: NetworkParameters): Try[BitcoinAddress] = {
    fromScriptPubKey(spk,networkParameters)
  }
}