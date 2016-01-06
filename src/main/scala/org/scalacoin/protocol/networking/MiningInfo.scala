package org.scalacoin.protocol.networking

/**
 * Created by Tom on 1/5/2016.
 */
trait MiningInfo {
  def blocks : Int
  def currentBlockSize : Int
  def currentBlockTx : Int
  def difficulty : Double
  def errors : String
  def genproclimit : Int
  def networkHashPS : BigInt
  def pooledTx : Int
  def testNet : Boolean
  def chain : String
  def generate : Boolean
}

case class MiningInfoImpl(blocks : Int, currentBlockSize: Int, currentBlockTx : Int, difficulty : Double,
 errors : String, genproclimit : Int, networkHashPS: BigInt, pooledTx : Int, testNet : Boolean,
 chain : String, generate : Boolean) extends MiningInfo