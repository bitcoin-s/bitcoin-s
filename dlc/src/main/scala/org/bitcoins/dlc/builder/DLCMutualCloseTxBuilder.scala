package org.bitcoins.dlc.builder

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{ContractInfo, OracleInfo}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script.{EmptyScriptSignature, ScriptPubKey}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.builder.{FilterDustFinalizer, RawTxBuilder}
import org.bitcoins.crypto.SchnorrDigitalSignature

import scala.concurrent.{ExecutionContext, Future}

case class DLCMutualCloseTxBuilder(
    oracleInfo: OracleInfo,
    offerFinalSPK: ScriptPubKey,
    offerOutcomes: ContractInfo,
    acceptFinalSPK: ScriptPubKey,
    acceptOutcomes: ContractInfo,
    fundingOutPoint: TransactionOutPoint) {
  private val OracleInfo(oraclePubKey, preCommittedR) = oracleInfo

  private val sigPubKeys = offerOutcomes.keys.map { msg =>
    msg -> oraclePubKey.computeSigPoint(msg.bytes, preCommittedR)
  }.toMap

  def getPayouts(
      oracleSig: SchnorrDigitalSignature): (CurrencyUnit, CurrencyUnit) = {
    sigPubKeys.find(_._2 == oracleSig.sig.getPublicKey) match {
      case Some((hash, _)) =>
        (offerOutcomes(hash), acceptOutcomes(hash))
      case None =>
        throw new IllegalArgumentException(
          s"Signature does not correspond to a possible outcome! $oracleSig")
    }
  }

  def buildMutualCloseTx(sig: SchnorrDigitalSignature)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    val builder = RawTxBuilder()

    val (offerPayout, acceptPayout) = getPayouts(sig)

    builder += TransactionOutput(offerPayout, offerFinalSPK)
    builder += TransactionOutput(acceptPayout, acceptFinalSPK)

    builder += TransactionInput(fundingOutPoint,
                                EmptyScriptSignature,
                                TransactionConstants.sequence)

    FilterDustFinalizer.buildTx(builder.result())
  }
}
