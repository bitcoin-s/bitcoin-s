package org.scalacoin.marshallers.mining

import org.scalacoin.protocol.mining.{GetMiningInfo, GetMiningInfoImpl}
import spray.json._

/**
 * Created by Tom on 1/13/2016.
 */
object MiningInfoMarshaller extends DefaultJsonProtocol {
  val blocksKey = "blocks"
  val currentBlockSizeKey = "currentblocksize"
  val currentBlockTxKey = "currentblocktx"
  val difficultyKey = "difficulty"
  val errorsKey = "errors"
  val genProcLimitKey = "genproclimit"
  val networkHashPerSecondKey = "networkhashps"
  val pooledTxKey = "pooledtx"
  val testNetKey = "testnet"
  val chainKey = "chain"
  val generateKey = "generate"

  implicit object MiningInfoFormatter extends RootJsonFormat[GetMiningInfo] {
    override def read(value : JsValue) : GetMiningInfo = {
      val obj = value.asJsObject
      val blocks = obj.fields(blocksKey).convertTo[Int]
      val currentBlockSize = obj.fields(currentBlockSizeKey).convertTo[Int]
      val currentBlockTx = obj.fields(currentBlockTxKey).convertTo[Int]
      val difficulty = obj.fields(difficultyKey).convertTo[Double]
      val errors = obj.fields(errorsKey).convertTo[String]
      val genProcLimit = obj.fields(genProcLimitKey).convertTo[Int]
      val networkHashPerSecond = obj.fields(networkHashPerSecondKey).convertTo[BigInt]
      val pooledTx = obj.fields(pooledTxKey).convertTo[Int]
      val testNet = obj.fields(testNetKey).convertTo[Boolean]
      val chain = obj.fields(chainKey).convertTo[String]
      val generate = obj.fields(generateKey).convertTo[Boolean]
      GetMiningInfoImpl(blocks, currentBlockSize,currentBlockTx,difficulty,errors, genProcLimit,networkHashPerSecond,pooledTx,testNet,chain,generate)
    }

    override def write(miningMeta : GetMiningInfo) : JsValue = {
      val m : Map[String, JsValue] = Map (
      blocksKey -> JsNumber(miningMeta.blocks),
      currentBlockSizeKey -> JsNumber(miningMeta.currentBlockSize),
      currentBlockTxKey -> JsNumber(miningMeta.currentBlockTx),
      difficultyKey -> JsNumber(miningMeta.difficulty),
      errorsKey -> JsString(miningMeta.errors),
      genProcLimitKey -> JsNumber(miningMeta.genProcLimit),
      networkHashPerSecondKey -> JsNumber(miningMeta.networkHashPerSecond),
      pooledTxKey -> JsNumber(miningMeta.pooledTx),
      testNetKey -> JsBoolean(miningMeta.testNet),
      chainKey -> JsString(miningMeta.chain),
      generateKey -> JsBoolean(miningMeta.generate)
      )
      JsObject(m)
    }
  }
}
