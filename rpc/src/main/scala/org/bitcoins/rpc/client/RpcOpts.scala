package org.bitcoins.rpc.client

import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import play.api.libs.json.{Json, Writes}
import org.bitcoins.rpc.serializers.JsonSerializers._

object RpcOpts {
  case class FundRawTransactionOptions(
      changeAddress: Option[BitcoinAddress] = None,
      changePosition: Option[Int] = None,
      includeWatching: Boolean = false,
      lockUnspents: Boolean = false,
      reverseChangeKey: Boolean = true,
      feeRate: Option[Bitcoins] = None,
      subtractFeeFromOutputs: Option[Array[Int]])

  implicit val fundRawTransactionOptionsWrites: Writes[
    FundRawTransactionOptions] = Json.writes[FundRawTransactionOptions]

  case class SignRawTransactionOutputParameter(
      txid: DoubleSha256Digest,
      vout: Int,
      scriptPubKey: ScriptPubKey,
      reedemScript: Option[ScriptPubKey] = None,
      amount: Bitcoins)

  implicit val signRawTransactionOutputParameterWrites: Writes[
    SignRawTransactionOutputParameter] =
    Json.writes[SignRawTransactionOutputParameter]

  case class ImportMultiRequest(
      scriptPubKey: ImportMultiAddress,
      timestamp: UInt32,
      reedemscript: Option[ScriptPubKey] = None,
      pubkeys: Option[Vector[ScriptPubKey]] = None,
      keys: Option[Vector[ECPrivateKey]] = None,
      internal: Option[Boolean] = None,
      watchonly: Option[Boolean] = None,
      label: Option[String] = None)

  case class ImportMultiAddress(address: BitcoinAddress)
}
