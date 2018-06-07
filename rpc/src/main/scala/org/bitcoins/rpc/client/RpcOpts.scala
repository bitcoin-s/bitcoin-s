package org.bitcoins.rpc.client

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Bitcoins
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
}
