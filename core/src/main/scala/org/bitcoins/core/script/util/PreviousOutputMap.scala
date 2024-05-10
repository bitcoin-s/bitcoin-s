package org.bitcoins.core.script.util

import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.util.MapWrapper
import org.bitcoins.core.wallet.utxo._

case class PreviousOutputMap(
    outputMap: Map[TransactionOutPoint, TransactionOutput])
    extends MapWrapper[TransactionOutPoint, TransactionOutput] {

  override protected val wrapped: Map[TransactionOutPoint, TransactionOutput] =
    outputMap
}

object PreviousOutputMap {

  val empty: PreviousOutputMap = PreviousOutputMap(Map.empty)

  def fromScriptSignatureParams(
      inputInfos: Seq[ScriptSignatureParams[InputInfo]]): PreviousOutputMap = {
    fromInputInfos(inputInfos.map(_.inputInfo))
  }

  def fromInputInfos(inputInfos: Seq[InputInfo]): PreviousOutputMap = {
    val inputMap = inputInfos.map { inputInfo =>
      inputInfo.outPoint -> inputInfo.output
    }.toMap

    PreviousOutputMap(inputMap)
  }
}
