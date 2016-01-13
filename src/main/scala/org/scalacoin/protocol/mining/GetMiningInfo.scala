package org.scalacoin.protocol.mining

/**
 * Created by Tom on 1/13/2016.
 */
trait GetMiningInfo {
  def blocks : Int
  def currentBlockSize : Int
  def currentBlockTx : Int
  def difficulty : Double
  def errors : String
  def genProcLimit : Int
  def networkHashPerSecond : BigInt
  def pooledTx : Int
  def testNet : Boolean
  def chain : String
  def generate : Boolean
}

case class GetMiningInfoImpl(blocks : Int, currentBlockSize : Int, currentBlockTx : Int, difficulty : Double, errors : String,
                              genProcLimit : Int, networkHashPerSecond : BigInt, pooledTx : Int, testNet : Boolean ,
                              chain : String, generate : Boolean) extends  GetMiningInfo
