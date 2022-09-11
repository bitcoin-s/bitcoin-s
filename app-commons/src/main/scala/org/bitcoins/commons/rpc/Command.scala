package org.bitcoins.commons.rpc

import grizzled.slf4j.Logging
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.LockUnspentOutputParameter
import org.bitcoins.commons.jsonmodels.cli.ContractDescriptorParser
import org.bitcoins.commons.serializers.JsonReaders
import org.bitcoins.commons.util.WalletNames
import org.bitcoins.core.api.dlc.wallet.db.DLCContactDb
import org.bitcoins.core.api.wallet.CoinSelectionAlgo
import org.bitcoins.core.crypto.{ExtPrivateKey, MnemonicCode}
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.hd.AddressType
import org.bitcoins.core.hd.AddressType.SegWit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.dlc.models.ContractDescriptor
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.AddressLabelTag
import org.bitcoins.crypto._
import scodec.bits.ByteVector
import ujson._

import java.net.{InetSocketAddress, URI}
import java.nio.file.Path
import scala.util.{Failure, Success, Try}

sealed trait CommandRpc

sealed trait CliCommand {
  def defaultPort: Int
}

trait ServerlessCliCommand extends CliCommand {
  override def defaultPort: Int = 9999
}

trait AppServerCliCommand extends CliCommand {
  override def defaultPort: Int = 9999
}

trait Broadcastable extends CliCommand {
  override def defaultPort: Int = 9999
}
trait SignDLCCliCommand extends AppServerCliCommand

trait AddDLCSigsCliCommand extends AppServerCliCommand

trait AcceptDLCCliCommand extends AppServerCliCommand

trait SendCliCommand extends AppServerCliCommand {
  def destination: BitcoinAddress
}

trait OracleServerCliCommand extends CliCommand {
  override def defaultPort: Int = 9998
}

object CliCommand {

  case object NoCommand extends CliCommand {
    override val defaultPort: Int = 9999
  }
}

case class GetNewAddress(labelOpt: Option[AddressLabelTag])
    extends CliCommand
    with AppServerCliCommand

object GetNewAddress extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetNewAddress] = {
    if (jsArr.value.length == 1) {
      val labelOpt = nullToOpt(jsArr.arr.head).map {
        case Str(str) =>
          AddressLabelTag(str)
        case value: Value =>
          throw Value.InvalidData(value, "Expected a String")
      }

      Try(GetNewAddress(labelOpt))
    } else if (jsArr.value.isEmpty) {
      Success(GetNewAddress(None))
    } else {
      sys.error(s"Too many argumements for GetNewAddress, got=$jsArr")
    }
  }
}

case class LockUnspent(
    unlock: Boolean,
    outputParam: Vector[LockUnspentOutputParameter])
    extends CliCommand
    with AppServerCliCommand

object LockUnspent extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[LockUnspent] = {
    jsArr.arr.toList match {
      case unlockJs :: outPointsJs :: Nil =>
        Try {
          val unlock = unlockJs.bool
          val outPoints = jsToLockUnspentOutputParameters(outPointsJs).toVector

          LockUnspent(unlock, outPoints)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class LabelAddress(address: BitcoinAddress, label: AddressLabelTag)
    extends CliCommand
    with AppServerCliCommand

object LabelAddress extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[LabelAddress] = {
    jsArr.arr.toList match {
      case addrJs :: labelJs :: Nil =>
        Try {
          val addr = jsToBitcoinAddress(addrJs)
          val label = AddressLabelTag(labelJs.str)

          LabelAddress(addr, label)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class GetAddressTags(address: BitcoinAddress)
    extends CliCommand
    with AppServerCliCommand

object GetAddressTags extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetAddressTags] = {
    jsArr.arr.toList match {
      case addrJs :: Nil =>
        Try {
          val addr = jsToBitcoinAddress(addrJs)

          GetAddressTags(addr)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class GetAddressLabel(address: BitcoinAddress)
    extends CliCommand
    with AppServerCliCommand

object GetAddressLabel extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetAddressLabel] = {
    jsArr.arr.toList match {
      case addrJs :: Nil =>
        Try {
          val addr = jsToBitcoinAddress(addrJs)

          GetAddressLabel(addr)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class DropAddressLabel(address: BitcoinAddress, label: String)
    extends CliCommand
    with AppServerCliCommand

object DropAddressLabel extends ServerJsonModels {

  def fromJsArr(jsonArr: ujson.Arr): Try[DropAddressLabel] = {
    jsonArr.arr.toList match {
      case address :: label :: Nil =>
        Try {
          val addr = jsToBitcoinAddress(address)
          DropAddressLabel(addr, label.str)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class DropAddressLabels(address: BitcoinAddress)
    extends CliCommand
    with AppServerCliCommand

object DropAddressLabels extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[DropAddressLabels] = {
    jsArr.arr.toList match {
      case addrJs :: Nil =>
        Try {
          val addr = jsToBitcoinAddress(addrJs)

          DropAddressLabels(addr)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class GetBalance(isSats: Boolean)
    extends CliCommand
    with AppServerCliCommand

object GetBalance extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetBalance] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(GetBalance(jsArr.arr.head.bool))
  }
}

case class GetConfirmedBalance(isSats: Boolean)
    extends CliCommand
    with AppServerCliCommand

object GetConfirmedBalance extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetConfirmedBalance] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(GetConfirmedBalance(jsArr.arr.head.bool))
  }
}

case class GetUnconfirmedBalance(isSats: Boolean)
    extends CliCommand
    with AppServerCliCommand

object GetUnconfirmedBalance extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetUnconfirmedBalance] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(GetUnconfirmedBalance(jsArr.arr.head.bool))
  }
}

case class GetAddressInfo(address: BitcoinAddress)
    extends CliCommand
    with AppServerCliCommand

object GetAddressInfo extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetAddressInfo] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    val address = jsToBitcoinAddress(jsArr.arr.head)

    Try(GetAddressInfo(address))
  }
}

case class SendRawTransaction(tx: Transaction)
    extends CliCommand
    with AppServerCliCommand

object SendRawTransaction extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SendRawTransaction] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(SendRawTransaction(jsToTx(jsArr.arr.head)))
  }
}

case class KeyManagerPassphraseChange(
    oldPassword: AesPassword,
    newPassword: AesPassword)
    extends CliCommand
    with AppServerCliCommand

object KeyManagerPassphraseChange extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[KeyManagerPassphraseChange] = {
    jsArr.arr.toList match {
      case oldPassJs :: newPassJs :: Nil =>
        Try {
          val oldPass = AesPassword.fromString(oldPassJs.str)
          val newPass = AesPassword.fromString(newPassJs.str)

          KeyManagerPassphraseChange(oldPass, newPass)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing old password and new password arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class KeyManagerPassphraseSet(password: AesPassword)
    extends CliCommand
    with AppServerCliCommand

object KeyManagerPassphraseSet extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[KeyManagerPassphraseSet] = {
    jsArr.arr.toList match {
      case passJs :: Nil =>
        Try {
          val pass = AesPassword.fromString(passJs.str)

          KeyManagerPassphraseSet(pass)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing password argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class ExportSeed(
    walletNameOpt: Option[String],
    passwordOpt: Option[AesPassword])
    extends CliCommand
    with AppServerCliCommand

object ExportSeed extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ExportSeed] = Try {
    val (walletNameOpt, passwordOpt) = jsToWalletNameAndPassword(jsArr)
    ExportSeed(walletNameOpt, passwordOpt)
  }
}

case class MarkSeedAsBackedUp(
    walletNameOpt: Option[String],
    passwordOpt: Option[AesPassword])
    extends CliCommand
    with AppServerCliCommand

object MarkSeedAsBackedUp extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[MarkSeedAsBackedUp] = Try {
    val (walletNameOpt, passwordOpt) = jsToWalletNameAndPassword(jsArr)
    MarkSeedAsBackedUp(walletNameOpt, passwordOpt)
  }
}

case class GetSeedBackupTime(
    walletNameOpt: Option[String],
    passwordOpt: Option[AesPassword])
    extends CliCommand
    with AppServerCliCommand

object GetSeedBackupTime extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetSeedBackupTime] = Try {
    val (walletNameOpt, passwordOpt) = jsToWalletNameAndPassword(jsArr)
    GetSeedBackupTime(walletNameOpt, passwordOpt)
  }
}

case class ImportSeed(
    walletNameOpt: Option[String],
    mnemonic: MnemonicCode,
    passwordOpt: Option[AesPassword])
    extends CliCommand
    with AppServerCliCommand

object ImportSeed extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ImportSeed] = {
    jsArr.arr.toList match {
      case walletNameJs :: mnemonicJs :: passJs :: Nil =>
        Try {
          val walletNameOpt = jsToWalletName(walletNameJs)
          val mnemonic = jsToMnemonics(mnemonicJs)
          val pass = jsToAESPassword(passJs)

          ImportSeed(walletNameOpt, mnemonic, pass)
        }
      case walletNameJs :: mnemonicJs :: Nil =>
        Try {
          val walletNameOpt = jsToWalletName(walletNameJs)
          val mnemonic = jsToMnemonics(mnemonicJs)

          ImportSeed(walletNameOpt, mnemonic, None)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing walletName, mnemonic, and password argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
    }
  }
}

case class ImportXprv(
    walletNameOpt: Option[String],
    xprv: ExtPrivateKey,
    passwordOpt: Option[AesPassword])
    extends CliCommand
    with AppServerCliCommand

object ImportXprv extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ImportXprv] = {
    jsArr.arr.toList match {
      case walletNameJs :: xprvJs :: passJs :: Nil =>
        Try {
          val walletNameOpt = jsToWalletName(walletNameJs)
          val xprv = ExtPrivateKey.fromString(xprvJs.str)
          val pass = jsToAESPassword(passJs)

          ImportXprv(walletNameOpt, xprv, pass)
        }
      case walletNameJs :: xprvJs :: Nil =>
        Try {
          val walletNameOpt = jsToWalletName(walletNameJs)
          val xprv = ExtPrivateKey.fromString(xprvJs.str)

          ImportXprv(walletNameOpt, xprv, None)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing walletName, xprv, and password argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
    }
  }
}

case class CreateMultisig(
    requiredKeys: Int,
    keys: Vector[ECPublicKey],
    addressType: AddressType)
    extends CliCommand
    with AppServerCliCommand

object CreateMultisig extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CreateMultisig] = {
    jsArr.arr.toList match {
      case requiredKeysJs :: keysJs :: addressTypeJs :: Nil =>
        Try {
          val requiredKeys = requiredKeysJs.num.toInt

          val keys = keysJs.arr.map(value => ECPublicKey(value.str))

          val addrType = AddressType.fromString(addressTypeJs.str)
          CreateMultisig(requiredKeys, keys.toVector, addrType)
        }
      case requiredKeysJs :: keysJs :: Nil =>
        Try {
          val requiredKeys = requiredKeysJs.num.toInt

          val keys = keysJs.arr.map(value => ECPublicKey(value.str))

          CreateMultisig(requiredKeys, keys.toVector, SegWit)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing requiredKeys, keys, and addressType argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
    }
  }
}

case class CombinePSBTs(psbts: Seq[PSBT])
    extends CliCommand
    with AppServerCliCommand

object CombinePSBTs extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CombinePSBTs] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(CombinePSBTs(jsToPSBTSeq(jsArr.arr.head)))
  }
}

case class JoinPSBTs(psbts: Seq[PSBT])
    extends CliCommand
    with AppServerCliCommand

object JoinPSBTs extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[JoinPSBTs] = {
    CombinePSBTs
      .fromJsArr(jsArr)
      .map(combine => JoinPSBTs(combine.psbts))
  }
}

case class FinalizePSBT(psbt: PSBT) extends CliCommand with AppServerCliCommand

object FinalizePSBT extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[FinalizePSBT] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(FinalizePSBT(jsToPSBT(jsArr.arr.head)))
  }
}

case class ExtractFromPSBT(psbt: PSBT)
    extends CliCommand
    with AppServerCliCommand

object ExtractFromPSBT extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ExtractFromPSBT] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(ExtractFromPSBT(jsToPSBT(jsArr.arr.head)))
  }
}

case class ConvertToPSBT(tx: Transaction)
    extends CliCommand
    with AppServerCliCommand

object ConvertToPSBT extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ConvertToPSBT] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(ConvertToPSBT(jsToTx(jsArr.arr.head)))
  }
}

case class GetBlockHeader(hash: DoubleSha256DigestBE)
    extends CliCommand
    with AppServerCliCommand

object GetBlockHeader extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetBlockHeader] =
    Try {
      require(jsArr.arr.size == 1,
              s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

      GetBlockHeader(DoubleSha256DigestBE(jsArr.arr.head.str))
    }
}

case class DecodeRawTransaction(tx: Transaction)
    extends CliCommand
    with AppServerCliCommand

object DecodeRawTransaction extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[DecodeRawTransaction] = {
    jsArr.arr.toList match {
      case tx :: Nil =>
        Try {
          DecodeRawTransaction(Transaction.fromHex(tx.str))
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class DecodePSBT(psbt: PSBT) extends CliCommand with AppServerCliCommand

object DecodePSBT extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[DecodePSBT] = {
    jsArr.arr.toList match {
      case psbtJs :: Nil =>
        Try {
          val psbt = jsToPSBT(psbtJs)
          DecodePSBT(psbt)
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class AnalyzePSBT(psbt: PSBT) extends CliCommand with AppServerCliCommand

object AnalyzePSBT extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[AnalyzePSBT] = {
    jsArr.arr.toList match {
      case psbtJs :: Nil =>
        Try {
          AnalyzePSBT(jsToPSBT(psbtJs))
        }
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class Rescan(
    batchSize: Option[Int],
    startBlock: Option[BlockStamp],
    endBlock: Option[BlockStamp],
    force: Boolean,
    ignoreCreationTime: Boolean)
    extends CliCommand
    with AppServerCliCommand

object Rescan extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[Rescan] = {

    def parseBlockStamp(value: Value): Option[BlockStamp] =
      nullToOpt(value).map {
        case Str(value) => BlockStamp.fromString(value)
        case Num(value) =>
          val int = value.toInt
          if (int >= 0 && int <= Int.MaxValue)
            BlockHeight(int)
          else throw Value.InvalidData(value, "Expected a positive integer")
        case _: Value =>
          throw Value.InvalidData(value, "Expected a Num or a Str")
      }

    def parseInt(value: Value): Option[Int] =
      nullToOpt(value).map {
        case Str(value) => value.toInt
        case Num(value) => value.toInt
        case _: Value =>
          throw Value.InvalidData(value, "Expected a Num or a Str")
      }

    def parseBoolean(value: Value): Boolean =
      value match {
        case Bool(bool) => bool
        case _: Value   => throw Value.InvalidData(value, "Expected a Bool")
      }

    jsArr.arr.toList match {
      case batchSizeJs :: startJs :: endJs :: forceJs :: ignoreCreationTimeJs :: Nil =>
        Try {
          val batchSize = parseInt(batchSizeJs)
          val start = parseBlockStamp(startJs)
          val end = parseBlockStamp(endJs)
          val force = parseBoolean(forceJs)
          val ignoreCreationTime = parseBoolean(ignoreCreationTimeJs)
          Rescan(batchSize = batchSize,
                 startBlock = start,
                 endBlock = end,
                 force = force,
                 ignoreCreationTime = ignoreCreationTime)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing addresses"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 5"))
    }
  }

}

case class GetTransaction(txId: DoubleSha256DigestBE)
    extends CliCommand
    with AppServerCliCommand

object GetTransaction extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetTransaction] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(GetTransaction(DoubleSha256DigestBE(jsArr.arr.head.str)))
  }
}

case class SendToAddress(
    destination: BitcoinAddress,
    amount: Bitcoins,
    satoshisPerVirtualByte: Option[SatoshisPerVirtualByte],
    noBroadcast: Boolean)
    extends CliCommand
    with Broadcastable
    with SendCliCommand

object SendToAddress extends ServerJsonModels {

  /// TODO do this in a more coherent fashion
  // custom akka-http directive?
  def fromJsArr(jsArr: ujson.Arr): Try[SendToAddress] = {
    jsArr.arr.toList match {
      case addrJs :: bitcoinsJs :: satsPerVBytesJs :: noBroadcastJs :: Nil =>
        Try {
          val address = jsToBitcoinAddress(addrJs)
          val bitcoins = Bitcoins(bitcoinsJs.num)
          val satoshisPerVirtualByte =
            nullToOpt(satsPerVBytesJs).map(satsPerVBytes =>
              SatoshisPerVirtualByte(Satoshis(satsPerVBytes.num.toLong)))
          val noBroadcast = noBroadcastJs.bool
          SendToAddress(address, bitcoins, satoshisPerVirtualByte, noBroadcast)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing address, amount, and fee rate arguments"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 4"))
    }
  }

}

case class GetDLCs(contactId: Option[InetSocketAddress])
    extends CliCommand
    with AppServerCliCommand

case object GetDLCs
    extends CliCommand
    with AppServerCliCommand
    with ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetDLCs] = {
    jsArr.arr.toList match {
      case addressJs :: Nil =>
        Try {
          val address = jsToInetSocketAddress(addressJs)
          GetDLCs(Some(address))
        }
      case Nil =>
        Try(GetDLCs(None))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class GetDLC(dlcId: Sha256Digest)
    extends CliCommand
    with AppServerCliCommand

object GetDLC extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetDLC] = {
    jsArr.arr.toList match {
      case paramHashJs :: Nil =>
        Try {
          val paramHash = Sha256Digest(paramHashJs.str)
          GetDLC(paramHash)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing paramHash argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class CreateDLCOffer(
    contractInfoTLV: ContractInfoV0TLV,
    collateral: Satoshis,
    feeRateOpt: Option[SatoshisPerVirtualByte],
    locktimeOpt: Option[UInt32],
    refundLocktime: UInt32,
    externalPayoutAddressOpt: Option[BitcoinAddress],
    externalChangeAddressOpt: Option[BitcoinAddress],
    peerAddressOpt: Option[InetSocketAddress])
    extends CliCommand
    with AppServerCliCommand

object CreateDLCOffer extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CreateDLCOffer] = {

    def parseParameters(
        contractInfoJs: Value,
        collateralJs: Value,
        feeRateOptJs: Value,
        locktimeJs: Value,
        refundLTJs: Value,
        payoutAddressJs: Value,
        changeAddressJs: Value,
        peerAddressJs: Value) = Try {
      val contractInfoTLV = jsToContractInfoTLV(contractInfoJs)
      val collateral = jsToSatoshis(collateralJs)
      val feeRate = jsToSatoshisPerVirtualByteOpt(feeRateOptJs)
      val locktimeJsOpt = nullToOpt(locktimeJs)
      val locktimeOpt = locktimeJsOpt.map(js => jsToUInt32(js))
      val refundLT = jsToUInt32(refundLTJs)
      val payoutAddressJsOpt = nullToOpt(payoutAddressJs)
      val payoutAddressOpt =
        payoutAddressJsOpt.map(js => jsToBitcoinAddress(js))
      val changeAddressJsOpt = nullToOpt(changeAddressJs)
      val changeAddressOpt =
        changeAddressJsOpt.map(js => jsToBitcoinAddress(js))
      val peerAddressJsOpt = nullToOpt(peerAddressJs)
      val peerAddressOpt = peerAddressJsOpt.map(js => jsToInetSocketAddress(js))

      CreateDLCOffer(contractInfoTLV,
                     collateral,
                     feeRate,
                     locktimeOpt,
                     refundLT,
                     payoutAddressOpt,
                     changeAddressOpt,
                     peerAddressOpt)
    }

    jsArr.arr.toList match {
      case contractInfoJs :: collateralJs :: feeRateOptJs :: locktimeJs :: refundLTJs :: Nil =>
        parseParameters(contractInfoJs,
                        collateralJs,
                        feeRateOptJs,
                        locktimeJs,
                        refundLTJs,
                        Null,
                        Null,
                        Null)
      case contractInfoJs :: collateralJs :: feeRateOptJs :: locktimeJs :: refundLTJs :: payoutAddressJs :: Nil =>
        parseParameters(contractInfoJs,
                        collateralJs,
                        feeRateOptJs,
                        locktimeJs,
                        refundLTJs,
                        payoutAddressJs,
                        Null,
                        Null)
      case contractInfoJs :: collateralJs :: feeRateOptJs :: locktimeJs :: refundLTJs :: payoutAddressJs :: changeAddressJs :: Nil =>
        parseParameters(contractInfoJs,
                        collateralJs,
                        feeRateOptJs,
                        locktimeJs,
                        refundLTJs,
                        payoutAddressJs,
                        changeAddressJs,
                        Null)
      case contractInfoJs :: collateralJs :: feeRateOptJs :: locktimeJs :: refundLTJs :: payoutAddressJs :: changeAddressJs :: peerAddressJs :: Nil =>
        parseParameters(contractInfoJs,
                        collateralJs,
                        feeRateOptJs,
                        locktimeJs,
                        refundLTJs,
                        payoutAddressJs,
                        changeAddressJs,
                        peerAddressJs)
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 6"))
    }
  }
}

case class DecodeContractInfo(contractInfo: ContractInfoV0TLV)
    extends CliCommand
    with AppServerCliCommand

object DecodeContractInfo extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[DecodeContractInfo] = {
    jsArr.arr.toList match {
      case contractInfoJs :: Nil =>
        Try {
          val contractInfo = ContractInfoV0TLV(contractInfoJs.str)
          DecodeContractInfo(contractInfo)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing contractInfo argument"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class DecodeOffer(offer: DLCOfferTLV)
    extends CliCommand
    with AppServerCliCommand

object DecodeOffer extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[DecodeOffer] = {
    jsArr.arr.toList match {
      case offerJs :: Nil =>
        Try {
          val offer: LnMessage[DLCOfferTLV] =
            LnMessageFactory(DLCOfferTLV).fromHex(offerJs.str)
          DecodeOffer(offer.tlv)
        } match {
          case Success(value) => Success(value)
          case Failure(_) =>
            Try {
              val offer = DLCOfferTLV.fromHex(offerJs.str)
              DecodeOffer(offer)
            }
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing offer argument"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class DecodeAnnouncement(announcement: OracleAnnouncementTLV)
    extends CliCommand
    with AppServerCliCommand

object DecodeAnnouncement extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[DecodeAnnouncement] = {
    jsArr.arr.toList match {
      case annJs :: Nil =>
        Try {
          val announcementTLV = OracleAnnouncementTLV(annJs.str)
          DecodeAnnouncement(announcementTLV)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing announcement argument"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class AcceptDLCOffer(
    offer: LnMessage[DLCOfferTLV],
    externalPayoutAddressOpt: Option[BitcoinAddress],
    externalChangeAddressOpt: Option[BitcoinAddress],
    peerAddress: Option[InetSocketAddress])
    extends CliCommand
    with AcceptDLCCliCommand

object AcceptDLCOffer extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[AcceptDLCOffer] = {
    def parseParameters(
        offerJs: Value,
        payoutAddressJs: Value,
        changeAddressJs: Value,
        peerAddressJs: Value): Try[AcceptDLCOffer] = Try {
      val offer = LnMessageFactory(DLCOfferTLV)
        .fromHexT(offerJs.str)
        .getOrElse(LnMessage(DLCOfferTLV.fromHex(offerJs.str)))
      val payoutAddressJsOpt = nullToOpt(payoutAddressJs)
      val payoutAddressOpt =
        payoutAddressJsOpt.map(js => jsToBitcoinAddress(js))
      val changeAddressJsOpt = nullToOpt(changeAddressJs)
      val changeAddressOpt =
        changeAddressJsOpt.map(js => jsToBitcoinAddress(js))
      val peerAddressJsOpt = nullToOpt(peerAddressJs)
      val peerAddress = peerAddressJsOpt.map(js => jsToInetSocketAddress(js))
      AcceptDLCOffer(offer, payoutAddressOpt, changeAddressOpt, peerAddress)
    }

    jsArr.arr.toList match {
      case offerJs :: Nil =>
        parseParameters(offerJs, Null, Null, Null)
      case offerJs :: payoutAddressJs :: Nil =>
        parseParameters(offerJs, payoutAddressJs, Null, Null)
      case offerJs :: payoutAddressJs :: changeAddressJs :: Nil =>
        parseParameters(offerJs, payoutAddressJs, changeAddressJs, Null)
      case offerJs :: payoutAddressJs :: changeAddressJs :: peerAddressJs :: Nil =>
        parseParameters(offerJs,
                        payoutAddressJs,
                        changeAddressJs,
                        peerAddressJs)
      case Nil =>
        Failure(new IllegalArgumentException("Missing offer argument"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class AcceptDLC(
    offer: LnMessage[DLCOfferTLV],
    peerAddr: InetSocketAddress,
    externalPayoutAddressOpt: Option[BitcoinAddress],
    externalChangeAddressOpt: Option[BitcoinAddress])
    extends CliCommand
    with AcceptDLCCliCommand

object AcceptDLC extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[AcceptDLC] = {
    def parseParameters(
        offerJs: Value,
        addrJs: Value,
        payoutAddressJs: Value,
        changeAddressJs: Value): Try[AcceptDLC] = Try {
      val lnMessageOfferT = LnMessageFactory(DLCOfferTLV)
        .fromHexT(offerJs.str)
      val offer: LnMessage[DLCOfferTLV] = lnMessageOfferT match {
        case Success(o) => o
        case Failure(_) => LnMessage(DLCOfferTLV.fromHex(offerJs.str))
      }

      val peerAddr = jsToInetSocketAddress(addrJs)
      val payoutAddressJsOpt = nullToOpt(payoutAddressJs)
      val payoutAddressOpt =
        payoutAddressJsOpt.map(js => jsToBitcoinAddress(js))
      val changeAddressJsOpt = nullToOpt(changeAddressJs)
      val changeAddressOpt =
        changeAddressJsOpt.map(js => jsToBitcoinAddress(js))

      AcceptDLC(offer, peerAddr, payoutAddressOpt, changeAddressOpt)
    }

    jsArr.arr.toList match {
      case offerJs :: addrJs :: Nil =>
        parseParameters(offerJs, addrJs, Null, Null)
      case offerJs :: addrJs :: payoutAddressJs :: Nil =>
        parseParameters(offerJs, addrJs, payoutAddressJs, Null)
      case offerJs :: addrJs :: payoutAddressJs :: changeAddressJs :: Nil =>
        parseParameters(offerJs, addrJs, payoutAddressJs, changeAddressJs)
      case Nil =>
        Failure(
          new IllegalArgumentException("Missing offer and peerAddr argument"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class SignDLCFromFile(path: Path, destination: Option[Path])
    extends CliCommand
    with SignDLCCliCommand

case class SignDLC(accept: LnMessage[DLCAcceptTLV])
    extends CliCommand
    with SignDLCCliCommand

object SignDLC extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SignDLC] = {
    jsArr.arr.toList match {
      case acceptJs :: Nil =>
        Try {
          val accept = LnMessageFactory(DLCAcceptTLV)
            .fromHexT(acceptJs.str)
            .getOrElse(LnMessage(DLCAcceptTLV.fromHex(acceptJs.str)))
          SignDLC(accept)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing accept argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class AddDLCSigs(sigs: LnMessage[DLCSignTLV])
    extends CliCommand
    with AddDLCSigsCliCommand

object AddDLCSigs extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[AddDLCSigs] = {
    jsArr.arr.toList match {
      case sigsJs :: Nil =>
        Try {
          val sigs = LnMessageFactory(DLCSignTLV)
            .fromHexT(sigsJs.str)
            .getOrElse(LnMessage(DLCSignTLV.fromHex(sigsJs.str)))

          AddDLCSigs(sigs)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing sigs argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class GetDLCFundingTx(contractId: ByteVector)
    extends CliCommand
    with AppServerCliCommand

object GetDLCFundingTx extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetDLCFundingTx] = {
    jsArr.arr.toList match {
      case contractIdJs :: Nil =>
        Try {
          val contractId = ByteVector.fromValidHex(contractIdJs.str)
          GetDLCFundingTx(contractId)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing contractId argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class BroadcastDLCFundingTx(contractId: ByteVector)
    extends CliCommand
    with AppServerCliCommand

object BroadcastDLCFundingTx extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[BroadcastDLCFundingTx] = {
    jsArr.arr.toList match {
      case contractIdJs :: Nil =>
        Try {
          val contractId = ByteVector.fromValidHex(contractIdJs.str)
          BroadcastDLCFundingTx(contractId)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing contractId argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class ExecuteDLC(
    contractId: ByteVector,
    oracleSigs: Vector[OracleAttestmentTLV],
    noBroadcast: Boolean)
    extends CliCommand
    with Broadcastable

object ExecuteDLC extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ExecuteDLC] = {
    jsArr.arr.toList match {
      case contractIdJs :: oracleSigsJs :: noBroadcastJs :: Nil =>
        Try {
          val contractId = ByteVector.fromValidHex(contractIdJs.str)
          val oracleSigs = jsToOracleAttestmentTLVVec(oracleSigsJs)
          val noBroadcast = noBroadcastJs.bool

          ExecuteDLC(contractId, oracleSigs, noBroadcast)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing contractId, oracleSigs, and noBroadcast arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
    }
  }
}

case class ExecuteDLCRefund(contractId: ByteVector, noBroadcast: Boolean)
    extends CliCommand
    with Broadcastable

object ExecuteDLCRefund extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[ExecuteDLCRefund] = {
    jsArr.arr.toList match {
      case contractIdJs :: noBroadcastJs :: Nil =>
        Try {
          val contractId = ByteVector.fromValidHex(contractIdJs.str)
          val noBroadcast = noBroadcastJs.bool

          ExecuteDLCRefund(contractId, noBroadcast)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing contractId argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class SendFromOutPoints(
    outPoints: Vector[TransactionOutPoint],
    destination: BitcoinAddress,
    amount: Bitcoins,
    feeRateOpt: Option[SatoshisPerVirtualByte])
    extends CliCommand
    with SendCliCommand

object SendFromOutPoints extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SendFromOutPoints] = {
    jsArr.arr.toList match {
      case outPointsJs :: addrJs :: bitcoinsJs :: satsPerVBytesJs :: Nil =>
        Try {
          val outPoints = jsToTransactionOutPointSeq(outPointsJs).toVector
          val address = jsToBitcoinAddress(addrJs)
          val bitcoins = Bitcoins(bitcoinsJs.num)
          val satoshisPerVirtualByte =
            nullToOpt(satsPerVBytesJs).map(satsPerVBytes =>
              SatoshisPerVirtualByte(Satoshis(satsPerVBytes.num.toLong)))
          SendFromOutPoints(outPoints,
                            address,
                            bitcoins,
                            satoshisPerVirtualByte)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing outPoints, address, amount, and fee rate arguments"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 4"))
    }
  }
}

case class SweepWallet(
    destination: BitcoinAddress,
    feeRateOpt: Option[SatoshisPerVirtualByte])
    extends CliCommand
    with SendCliCommand
    with ServerJsonModels

object SweepWallet extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SweepWallet] = {
    jsArr.arr.toList match {
      case addrJs :: satsPerVBytesJs :: Nil =>
        Try {
          val destination = jsToBitcoinAddress(addrJs)
          val satoshisPerVirtualByte =
            nullToOpt(satsPerVBytesJs).map(satsPerVBytes =>
              SatoshisPerVirtualByte(Satoshis(satsPerVBytes.num.toLong)))
          SweepWallet(destination, satoshisPerVirtualByte)
        }
      case addrJs :: Nil =>
        Try {
          val destination = jsToBitcoinAddress(addrJs)
          SweepWallet(destination, None)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing address and fee rate arguments"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class SendWithAlgo(
    destination: BitcoinAddress,
    amount: Bitcoins,
    feeRateOpt: Option[SatoshisPerVirtualByte],
    algo: CoinSelectionAlgo)
    extends CliCommand
    with SendCliCommand
    with ServerJsonModels

object SendWithAlgo extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SendWithAlgo] = {
    jsArr.arr.toList match {
      case addrJs :: bitcoinsJs :: satsPerVBytesJs :: algoJs :: Nil =>
        Try {
          val address = jsToBitcoinAddress(addrJs)
          val bitcoins = Bitcoins(bitcoinsJs.num)
          val satoshisPerVirtualByte =
            nullToOpt(satsPerVBytesJs).map(satsPerVBytes =>
              SatoshisPerVirtualByte(Satoshis(satsPerVBytes.num.toLong)))
          val algo = jsToCoinSelectionAlgo(algoJs)

          SendWithAlgo(address, bitcoins, satoshisPerVirtualByte, algo)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing address, amount, fee rate, and algo arguments"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 4"))
    }
  }

}

case class SignPSBT(psbt: PSBT)
    extends CliCommand
    with AppServerCliCommand
    with ServerJsonModels

object SignPSBT extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SignPSBT] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")

    Try(SignPSBT(jsToPSBT(jsArr.arr.head)))
  }
}

case class OpReturnCommit(
    message: String,
    hashMessage: Boolean,
    feeRateOpt: Option[SatoshisPerVirtualByte])
    extends CliCommand
    with AppServerCliCommand
    with ServerJsonModels

object OpReturnCommit extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[OpReturnCommit] = {
    jsArr.arr.toList match {
      case messageJs :: hashMessageJs :: feeRateOptJs :: Nil =>
        Try {
          val message = messageJs.str
          val hashMessage = hashMessageJs.bool
          val feeRateOpt =
            nullToOpt(feeRateOptJs).map(satsPerVBytes =>
              SatoshisPerVirtualByte(Satoshis(satsPerVBytes.num.toLong)))
          OpReturnCommit(message, hashMessage, feeRateOpt)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing message, hashMessage, and fee rate arguments"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 3"))
    }
  }
}
case class BumpFee(txId: DoubleSha256DigestBE, feeRate: SatoshisPerVirtualByte)

case class BumpFeeCPFP(
    txId: DoubleSha256DigestBE,
    feeRate: SatoshisPerVirtualByte)
    extends AppServerCliCommand

case class BumpFeeRBF(
    txId: DoubleSha256DigestBE,
    feeRate: SatoshisPerVirtualByte)
    extends AppServerCliCommand

object BumpFee extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[BumpFee] = {
    jsArr.arr.toList match {
      case txIdJs :: feeRateJs :: Nil =>
        Try {
          val txId = DoubleSha256DigestBE(txIdJs.str)
          val feeRate = SatoshisPerVirtualByte(Satoshis(feeRateJs.num.toLong))
          BumpFee(txId, feeRate)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException("Missing txId and fee rate arguments"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class CreateContractInfo(
    announcementTLV: OracleAnnouncementTLV,
    totalCollateral: Satoshis,
    contractDescriptor: ContractDescriptorTLV)
    extends CommandRpc
    with AppServerCliCommand
    with ServerJsonModels {

  def ContractDescriptorTLV: ContractDescriptor = {
    ContractDescriptor.fromTLV(contractDescriptor)
  }
}

object CreateContractInfo extends ServerJsonModels {

  lazy val empty: CreateContractInfo = {
    CreateContractInfo(announcementTLV = OracleAnnouncementV0TLV.dummy,
                       totalCollateral = Satoshis.zero,
                       contractDescriptor = ContractDescriptorTLV.empty)
  }

  def fromJsArr(arr: ujson.Arr): Try[CreateContractInfo] = {
    arr.arr.toVector match {
      case announcementVal +: totalCollateralVal +: payoutsVal +: Vector() =>
        Try {
          val announcementTLV =
            OracleAnnouncementTLV.fromHex(announcementVal.str)
          val totalCollateral = Satoshis(totalCollateralVal.num.toLong)
          //validate that these are part of the announcement?
          val contractDescriptor =
            ContractDescriptorParser.parseCmdLine(payoutsVal, announcementTLV)

          CreateContractInfo(announcementTLV,
                             totalCollateral,
                             contractDescriptor)
        }
      case other =>
        val exn = new IllegalArgumentException(
          s"Bad number or arguments to createcontractinfo, got=${other.length} expected=3")
        Failure(exn)
    }
  }
}

case class ContactAdd(alias: String, address: InetSocketAddress, memo: String)
    extends CommandRpc
    with AppServerCliCommand
    with ServerJsonModels {
  def toDLCContactDb: DLCContactDb = DLCContactDb(alias, address, memo)
}

object ContactAdd {

  val empty: ContactAdd =
    ContactAdd("", InetSocketAddress.createUnresolved("127.0.0.1", 9999), "")

  def fromJsArr(arr: ujson.Arr): Try[ContactAdd] = {
    arr.arr.toList match {
      case aliasJs :: addressJs :: memoJs :: Nil =>
        Try {
          val address = {
            val uri = new URI(s"tcp://${addressJs.str}")
            InetSocketAddress.createUnresolved(uri.getHost, uri.getPort)
          }
          ContactAdd(aliasJs.str, address, memoJs.str)
        }
      case other =>
        val exn = new IllegalArgumentException(
          s"Bad number or arguments to contact-add, got=${other.length} expected=3")
        Failure(exn)
    }
  }
}

case object ContactsList extends CommandRpc with AppServerCliCommand

case class ContactRemove(address: InetSocketAddress)
    extends CommandRpc
    with AppServerCliCommand
    with ServerJsonModels

object ContactRemove {

  def fromJsArr(arr: ujson.Arr): Try[ContactRemove] = {
    arr.arr.toList match {
      case addressJs :: Nil =>
        Try {
          val address = {
            val uri = new URI(s"tcp://${addressJs.str}")
            InetSocketAddress.createUnresolved(uri.getHost, uri.getPort)
          }
          ContactRemove(address)
        }
      case other =>
        val exn = new IllegalArgumentException(
          s"Bad number or arguments to contact-remove, got=${other.length} expected=1")
        Failure(exn)
    }
  }
}

case class DLCContactAdd(dlcId: Sha256Digest, address: InetSocketAddress)
    extends CommandRpc
    with AppServerCliCommand
    with ServerJsonModels

object DLCContactAdd {

  val empty: DLCContactAdd =
    DLCContactAdd(Sha256Digest.empty,
                  InetSocketAddress.createUnresolved("127.0.0.1", 9999))

  def fromJsArr(arr: ujson.Arr): Try[DLCContactAdd] = {
    arr.arr.toList match {
      case dlcIdJs :: addressJs :: Nil =>
        Try {
          val dlcId = Sha256Digest.fromHex(dlcIdJs.str)
          val address = {
            val uri = new URI(s"tcp://${addressJs.str}")
            InetSocketAddress.createUnresolved(uri.getHost, uri.getPort)
          }
          DLCContactAdd(dlcId, address)
        }
      case other =>
        val exn = new IllegalArgumentException(
          s"Bad number or arguments to dlc-contact-add, got=${other.length} expected=2")
        Failure(exn)
    }
  }
}

case class DLCContactRemove(dlcId: Sha256Digest)
    extends CommandRpc
    with AppServerCliCommand

object DLCContactRemove {

  def fromJsArr(arr: ujson.Arr): Try[DLCContactRemove] = {
    arr.arr.toList match {
      case dlcIdJs :: Nil =>
        Try {
          val dlcId = Sha256Digest.fromHex(dlcIdJs.str)
          DLCContactRemove(dlcId)
        }
      case other =>
        val exn = new IllegalArgumentException(
          s"Bad number or arguments to contact-remove, got=${other.length} expected=1")
        Failure(exn)
    }
  }
}

case class DLCCheckConnection(address: InetSocketAddress)
    extends CommandRpc
    with AppServerCliCommand
    with ServerJsonModels

object DLCCheckConnection {

  def fromJsArr(arr: ujson.Arr): Try[DLCCheckConnection] = {
    arr.arr.toList match {
      case addressJs :: Nil =>
        Try {
          val address = {
            val uri = new URI(s"tcp://${addressJs.str}")
            InetSocketAddress.createUnresolved(uri.getHost, uri.getPort)
          }
          DLCCheckConnection(address)
        }
      case other =>
        val exn = new IllegalArgumentException(
          s"Bad number or arguments to checkconnection, got=${other.length} expected=1")
        Failure(exn)
    }
  }
}

case class LoadWallet(
    walletNameOpt: Option[String],
    passwordOpt: Option[AesPassword],
    bip39PasswordOpt: Option[String])
    extends CommandRpc
    with AppServerCliCommand

object LoadWallet extends ServerJsonModels with Logging {

  def fromJsArr(arr: ujson.Arr): Try[LoadWallet] = Try {
    arr.arr.toList match {
      case _ :: _ :: bip39PasswordJs :: Nil =>
        val (walletNameOpt, passwordOpt) =
          jsToWalletNameAndPassword(arr.arr.slice(0, 2))
        LoadWallet(walletNameOpt,
                   passwordOpt,
                   nullToOpt(bip39PasswordJs).map(_.str))
      case _ :: _ :: Nil =>
        val (walletNameOpt, passwordOpt) =
          jsToWalletNameAndPassword(arr.arr.slice(0, 2))
        LoadWallet(walletNameOpt, passwordOpt, None)
      case walletNameJs :: Nil =>
        LoadWallet(jsToStringOpt(walletNameJs), None, None)
      case Nil =>
        LoadWallet(None, None, None)
      case other =>
        throw new IllegalArgumentException(
          s"Bad number of arguments: ${other.length}. Expected: 3")
    }
  }
}

trait ServerJsonModels {

  def jsToOracleAnnouncementTLV(js: Value): OracleAnnouncementTLV =
    js match {
      case str: Str =>
        OracleAnnouncementTLV(str.value)
      case _: Value =>
        throw Value.InvalidData(
          js,
          "Expected an OracleAnnouncementTLV as a hex string")
    }

  def jsToContractInfoTLV(js: Value): ContractInfoV0TLV =
    js match {
      case str: Str =>
        ContractInfoV0TLV(str.value)
      case _: Value =>
        throw Value.InvalidData(js, "Expected a ContractInfo as a hex string")
    }

  def jsToSatoshisPerVirtualByteOpt(js: Value): Option[SatoshisPerVirtualByte] =
    nullToOpt(js).map {
      case str: Str =>
        SatoshisPerVirtualByte(Satoshis(str.value))
      case num: Num =>
        SatoshisPerVirtualByte(Satoshis(num.value.toLong))
      case _: Value =>
        throw Value.InvalidData(js, "Expected a fee rate in sats/vbyte")
    }

  def jsToUInt32(js: Value): UInt32 =
    js match {
      case str: Str =>
        UInt32(BigInt(str.value))
      case num: Num =>
        UInt32(num.value.toLong)
      case _: Value =>
        throw Value.InvalidData(js, "Expected a UInt32")
    }

  def jsToSatoshis(js: Value): Satoshis = JsonReaders.jsToSatoshis(js)

  def jsToBitcoinAddress(js: Value): BitcoinAddress = {
    try {
      BitcoinAddress.fromString(js.str)
    } catch {
      case _: IllegalArgumentException =>
        throw Value.InvalidData(js, "Expected a valid address")
    }
  }

  def jsToPSBTSeq(js: Value): Seq[PSBT] = {
    js.arr.foldLeft(Seq.empty[PSBT])((seq, psbt) => seq :+ jsToPSBT(psbt))
  }

  def jsToPSBT(js: Value): PSBT = PSBT.fromString(js.str)

  def jsToTransactionOutPointSeq(js: Value): Seq[TransactionOutPoint] = {
    js.arr.foldLeft(Seq.empty[TransactionOutPoint])((seq, outPoint) =>
      seq :+ jsToTransactionOutPoint(outPoint))
  }

  def jsToTransactionOutPoint(js: Value): TransactionOutPoint =
    TransactionOutPoint(js.str)

  def jsToLockUnspentOutputParameter(js: Value): LockUnspentOutputParameter =
    LockUnspentOutputParameter.fromJson(js)

  def jsToLockUnspentOutputParameters(
      js: Value): Seq[LockUnspentOutputParameter] = {
    js.arr.foldLeft(Seq.empty[LockUnspentOutputParameter])((seq, outPoint) =>
      seq :+ jsToLockUnspentOutputParameter(outPoint))
  }

  def jsToCoinSelectionAlgo(js: Value): CoinSelectionAlgo =
    CoinSelectionAlgo
      .fromString(js.str)

  def jsToTx(js: Value): Transaction = Transaction.fromHex(js.str)

  def nullToOpt(value: Value): Option[Value] =
    value match {
      case Null                      => None
      case Arr(arr) if arr.isEmpty   => None
      case Arr(arr) if arr.size == 1 => Some(arr.head)
      case _: Value                  => Some(value)
    }

  def jsToSchnorrDigitalSignature(js: Value): SchnorrDigitalSignature =
    js match {
      case str: Str =>
        SchnorrDigitalSignature(str.value)
      case _: Value =>
        throw Value.InvalidData(
          js,
          "Expected a SchnorrDigitalSignature as a hex string")
    }

  def jsToSchnorrDigitalSignatureVec(
      js: Value): Vector[SchnorrDigitalSignature] = {
    js.arr.foldLeft(Vector.empty[SchnorrDigitalSignature])((vec, sig) =>
      vec :+ jsToSchnorrDigitalSignature(sig))
  }

  def jsToOracleAttestmentTLV(js: Value): OracleAttestmentTLV =
    js match {
      case str: Str =>
        OracleAttestmentTLV(str.value)
      case _: Value =>
        throw Value.InvalidData(
          js,
          "Expected a OracleAttestmentTLV as a hex string")
    }

  def jsToOracleAttestmentTLVVec(js: Value): Vector[OracleAttestmentTLV] = {
    js.arr.foldLeft(Vector.empty[OracleAttestmentTLV])((vec, tlv) =>
      vec :+ jsToOracleAttestmentTLV(tlv))
  }

  def jsToAESPassword(js: Value): Option[AesPassword] = {
    js match {
      case Str(str) =>
        Some(AesPassword.fromString(str))
      case Null =>
        None
      case Arr(_) | False | True | Num(_) | Obj(_) =>
        throw new IllegalArgumentException("password must be a string or null")
    }
  }

  def jsToStringOpt(js: Value): Option[String] = {
    js match {
      case Str(str) =>
        Some(str)
      case Null =>
        None
      case Arr(_) | False | True | Num(_) | Obj(_) =>
        throw new IllegalArgumentException("password must be a string or null")
    }
  }

  def jsToWalletName(js: Value): Option[String] = {
    val walletNameOpt = jsToStringOpt(js)
    if (!walletNameOpt.forall(_.length <= WalletNames.walletNameMaxLen)) {
      throw new IllegalArgumentException(
        s"Invalid wallet name length: ${walletNameOpt.map(_.length).getOrElse(0)}. Max length is ${WalletNames.walletNameMaxLen}.")
    }
    if (!walletNameOpt.forall(WalletNames.validateWalletName)) {
      throw new IllegalArgumentException(
        s"Invalid wallet name `${walletNameOpt.getOrElse("")}`.")
    }
    walletNameOpt
  }

  def jsToWalletNameAndPassword(
      js: Value): (Option[String], Option[AesPassword]) = {
    js match {
      case Arr(arr) =>
        arr.toList match {
          case walletNameJs :: passJs :: Nil =>
            (jsToWalletName(walletNameJs), jsToAESPassword(passJs))
          case walletNameJs :: Nil =>
            (jsToWalletName(walletNameJs), None)
          case Nil =>
            (None, None)
          case other =>
            throw new IllegalArgumentException(
              s"Bad number of arguments: ${other.length}. Expected: 2")
        }
      case _: Value =>
        throw new IllegalArgumentException(s"Expected json.Arr")
    }
  }

  def jsToMnemonics(js: Value): MnemonicCode = {
    val mnemonicWords = js match {
      case Str(str) => str.split(' ').toVector
      case Arr(arr) => arr.map(_.str).toVector
      case Null | False | True | Num(_) | Obj(_) =>
        throw new IllegalArgumentException(
          "mnemonic must be a string or array of strings")
    }
    MnemonicCode.fromWords(mnemonicWords)
  }

  def jsToInetSocketAddress(
      js: Value,
      defaultPort: Int = -1): InetSocketAddress = {
    js match {
      case str: Str =>
        val uri = new URI("tcp://" + str.str)
        val port = if (uri.getPort >= 0) uri.getPort else defaultPort
        InetSocketAddress.createUnresolved(uri.getHost, port)
      case _: Value =>
        throw Value.InvalidData(js, "Expected a host address")
    }
  }
}
