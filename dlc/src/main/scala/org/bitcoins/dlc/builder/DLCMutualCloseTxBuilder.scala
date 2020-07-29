package org.bitcoins.dlc.builder

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{ContractInfo, OracleInfo}
import org.bitcoins.core.protocol.script.{EmptyScriptSignature, ScriptPubKey}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.builder.{FilterDustFinalizer, RawTxBuilder}
import org.bitcoins.crypto.SchnorrDigitalSignature

import scala.concurrent.{ExecutionContext, Future}

/** Responsible for constructing unsigned DLC mutual close transactions */
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

  /** Constructs an unsigned mutual close transaction given an oracle signature */
  def buildMutualCloseTx(sig: SchnorrDigitalSignature)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val builder = RawTxBuilder()

    val (offerPayout, acceptPayout) =
      DLCTxBuilder.getPayouts(sig, sigPubKeys, offerOutcomes, acceptOutcomes)

    builder += TransactionOutput(offerPayout, offerFinalSPK)
    builder += TransactionOutput(acceptPayout, acceptFinalSPK)

    builder += TransactionInput(fundingOutPoint,
                                EmptyScriptSignature,
                                TransactionConstants.sequence)

    FilterDustFinalizer.buildTx(builder.result())
  }
}
