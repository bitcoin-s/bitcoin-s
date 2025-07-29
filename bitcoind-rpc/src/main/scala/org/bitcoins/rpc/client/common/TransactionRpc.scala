package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.FeeEstimationMode
import org.bitcoins.commons.jsonmodels.bitcoind.*
import org.bitcoins.commons.serializers.JsonSerializers.*
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.rpc.client.common.BitcoindVersion.{V27, V28, V29}
import play.api.libs.json.*

import scala.concurrent.Future

/** This trait defines RPC calls related to transactions in Bitcoin Core. These
  * RPC calls generally provide a higher level of abstraction than the ones
  * found in
  * [[org.bitcoins.rpc.client.common.RawTransactionRpc RawTransactionRpc]].
  */
trait TransactionRpc { self: Client =>

  def abandonTransaction(txid: DoubleSha256DigestBE): Future[Unit] = {
    bitcoindCall[Unit]("abandontransaction", List(JsString(txid.hex)))
  }

  def abandonTransaction(txid: DoubleSha256Digest): Future[Unit] = {
    abandonTransaction(txid.flip)
  }

  def bumpFee(
      txid: DoubleSha256DigestBE,
      confTarget: Int = 6,
      totalFee: Option[Satoshis] = None,
      replaceable: Boolean = true,
      estimateMode: String = "UNSET"
  ): Future[BumpFeeResult] = {
    val optionsNoFee =
      Map(
        "confTarget" -> JsNumber(confTarget),
        "replaceable" -> JsBoolean(replaceable),
        "estimate_mode" -> JsString(estimateMode)
      )

    val options = totalFee match {
      case Some(fee) =>
        optionsNoFee + ("totalFee" -> JsNumber(fee.toBigDecimal))
      case None => optionsNoFee
    }

    bitcoindCall[BumpFeeResult](
      "bumpfee",
      List(JsString(txid.hex), JsObject(options))
    )
  }

  def bumpFee(
      txid: DoubleSha256Digest,
      confTarget: Int,
      totalFee: Option[Satoshis],
      replaceable: Boolean,
      estimateMode: String
  ): Future[BumpFeeResult] = {
    bumpFee(txid.flip, confTarget, totalFee, replaceable, estimateMode)
  }

  // Needs manual testing!
  def estimateSmartFee(
      blocks: Int,
      mode: FeeEstimationMode = FeeEstimationMode.Ecnomical
  ): Future[EstimateSmartFeeResult] = {
    bitcoindCall[EstimateSmartFeeResult](
      "estimatesmartfee",
      List(JsNumber(blocks), JsString(mode.toString))
    )
  }

  def getTransaction(
      txid: DoubleSha256DigestBE,
      watchOnly: Boolean = false,
      walletName: String = BitcoindRpcClient.DEFAULT_WALLET_NAME
  ): Future[GetTransactionResult] = {
    self.version.flatMap {
      case V27 | BitcoindVersion.Unknown =>
        bitcoindCall[GetTransactionResultPreV28](
          "gettransaction",
          List(JsString(txid.hex), JsBoolean(watchOnly)),
          uriExtensionOpt = Some(walletExtension(walletName))
        )
      case V28 | V29 =>
        bitcoindCall[GetTransactionResultV28](
          "gettransaction",
          List(JsString(txid.hex), JsBoolean(watchOnly)),
          uriExtensionOpt = Some(walletExtension(walletName))
        )
    }
  }

  def getTxOut(
      txid: DoubleSha256DigestBE,
      vout: Long,
      includeMemPool: Boolean = true
  ): Future[Option[GetTxOutResult]] = {
    bitcoindCall[Option[GetTxOutResultV22]](
      "gettxout",
      List(JsString(txid.hex), JsNumber(vout), JsBoolean(includeMemPool))
    )
  }

  private def getTxOutProof(
      txids: Vector[DoubleSha256DigestBE],
      headerHash: Option[DoubleSha256DigestBE]
  ): Future[MerkleBlock] = {
    val params = {
      val hashes = JsArray(txids.map(hash => JsString(hash.hex)))
      if (headerHash.isEmpty) {
        List(hashes)
      } else {
        List(hashes, JsString(headerHash.get.hex))
      }
    }
    bitcoindCall[MerkleBlock]("gettxoutproof", params)
  }

  def getTxOutProof(txids: Vector[DoubleSha256DigestBE]): Future[MerkleBlock] =
    getTxOutProof(txids, None)

  def getTxOutProof(
      txids: Vector[DoubleSha256Digest],
      headerHash: DoubleSha256Digest
  ): Future[MerkleBlock] =
    getTxOutProof(txids.map(_.flip), Some(headerHash.flip))

  def getTxOutProof(
      txids: Vector[DoubleSha256DigestBE],
      headerHash: DoubleSha256DigestBE
  ): Future[MerkleBlock] =
    getTxOutProof(txids, Some(headerHash))

  def verifyTxOutProof(
      proof: MerkleBlock
  ): Future[Vector[DoubleSha256DigestBE]] = {
    bitcoindCall[Vector[DoubleSha256DigestBE]](
      "verifytxoutproof",
      List(JsString(proof.hex))
    )
  }

  def getTxOutSetInfo: Future[GetTxOutSetInfoResult] = {
    bitcoindCall[GetTxOutSetInfoResult]("gettxoutsetinfo")
  }
}
