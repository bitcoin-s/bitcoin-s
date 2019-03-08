package org.bitcoins.rpc.client.common

import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.rpc.serializers.JsonWriters._
import play.api.libs.json.{Json, Writes}

object RpcOpts {

  case class WalletCreateFundedPsbtOptions(
      changeAddress: Option[BitcoinAddress] = None,
      changePosition: Option[Int] = None,
      changeType: Option[AddressType] = None,
      includeWatching: Boolean = false,
      lockUnspents: Boolean = false,
      feeRate: Option[Bitcoins] = None,
      subtractFeeFromOutputs: Option[Vector[Int]] = None,
      replaceable: Boolean = false,
      confTarget: Option[Int] = None,
      estimateMode: FeeEstimationMode = FeeEstimationMode.Unset
  )

  case class FundRawTransactionOptions(
      changeAddress: Option[BitcoinAddress] = None,
      changePosition: Option[Int] = None,
      includeWatching: Boolean = false,
      lockUnspents: Boolean = false,
      reverseChangeKey: Boolean = true,
      feeRate: Option[Bitcoins] = None,
      subtractFeeFromOutputs: Option[Vector[Int]])

  sealed abstract class FeeEstimationMode

  object FeeEstimationMode {
    case object Unset extends FeeEstimationMode {
      override def toString: String = "UNSET"
    }
    case object Ecnomical extends FeeEstimationMode {
      override def toString: String = "ECONOMICAL"
    }
    case object Conservative extends FeeEstimationMode {
      override def toString: String = "CONSERVATIVE"
    }
  }

  implicit val fundRawTransactionOptionsWrites: Writes[
    FundRawTransactionOptions] = Json.writes[FundRawTransactionOptions]

  case class SignRawTransactionOutputParameter(
      txid: DoubleSha256Digest,
      vout: Int,
      scriptPubKey: ScriptPubKey,
      redeemScript: Option[ScriptPubKey] = None,
      amount: Bitcoins)

  implicit val signRawTransactionOutputParameterWrites: Writes[
    SignRawTransactionOutputParameter] =
    Json.writes[SignRawTransactionOutputParameter]

  case class ImportMultiRequest(
      scriptPubKey: ImportMultiAddress,
      timestamp: UInt32,
      redeemscript: Option[ScriptPubKey] = None,
      pubkeys: Option[Vector[ScriptPubKey]] = None,
      keys: Option[Vector[ECPrivateKey]] = None,
      internal: Option[Boolean] = None,
      watchonly: Option[Boolean] = None,
      label: Option[String] = None)

  case class ImportMultiAddress(address: BitcoinAddress)

  case class LockUnspentOutputParameter(txid: DoubleSha256Digest, vout: Int)

  implicit val lockUnspentParameterWrites: Writes[LockUnspentOutputParameter] =
    Json.writes[LockUnspentOutputParameter]

  sealed trait AddNodeArgument

  object AddNodeArgument {

    case object Add extends AddNodeArgument {
      override def toString: String = "add"
    }

    case object Remove extends AddNodeArgument {
      override def toString: String = "remove"
    }

    case object OneTry extends AddNodeArgument {
      override def toString: String = "onetry"
    }

  }

  sealed trait AddressType

  object AddressType {
    case object Legacy extends AddressType {
      override def toString: String = "legacy"
    }

    case object P2SHSegwit extends AddressType {
      override def toString: String = "p2sh-segwit"
    }

    case object Bech32 extends AddressType {
      override def toString: String = "bech32"
    }
  }

  sealed trait LabelPurpose

  object LabelPurpose {
    case object Send extends LabelPurpose {
      override def toString: String = "send"
    }
    case object Receive extends LabelPurpose {
      override def toString: String = "receive"
    }
  }

  case class BlockTemplateRequest(
      mode: String,
      capabilities: Vector[String],
      rules: Vector[String])

  implicit val blockTemplateRequest: Writes[BlockTemplateRequest] =
    Json.writes[BlockTemplateRequest]
}
