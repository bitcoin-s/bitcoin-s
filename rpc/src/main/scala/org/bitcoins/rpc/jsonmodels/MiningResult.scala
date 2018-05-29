package org.bitcoins.rpc.jsonmodels

sealed abstract class MiningResult

case class GetMiningInfoResult(
                                blocks: Int,
                                currentblockweight: Int,
                                currentblocktx: Int,
                                difficulty: Double,
                                networkhashps: Double,
                                pooledtx: Int,
                                chain: String,
                                warnings: String) extends MiningResult

