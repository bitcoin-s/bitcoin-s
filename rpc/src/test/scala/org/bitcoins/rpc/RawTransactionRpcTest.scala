package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{ P2SHScriptSignature, ScriptPubKey }
import org.bitcoins.core.protocol.transaction.{ TransactionInput, TransactionOutPoint }
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.common.{ BitcoindRpcClient, RpcOpts }
import org.scalatest.{ AsyncFlatSpec, BeforeAndAfterAll }
import org.slf4j.Logger

import scala.async.Async.{ async, await }
import scala.concurrent.ExecutionContext

class RawTransactionRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem = ActorSystem("RawTransactionRpcTest_ActorSystem")
  implicit val m: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContext = m.executionContext
  implicit val networkParam: NetworkParameters = TestUtil.network

  val client = new BitcoindRpcClient(TestUtil.instance())
  val otherClient = new BitcoindRpcClient(TestUtil.instance())

  val logger: Logger = BitcoinSLogger.logger

  override protected def beforeAll(): Unit = {
    logger.info("Starting RawTransactionRpcTest")
    logger.info("Bitcoin servers starting")
    TestUtil.startServers(client, otherClient)
    logger.info("Bitcoin servers started")
  }

  override protected def afterAll(): Unit = async {
    logger.info("Cleaning up after RawTransactionRpcTest")
    logger.info("Stopping Bitcoin servers")
    TestUtil.stopServers(client, otherClient)
    logger.info("Bitcoin servers stopped")

    logger.info("Stopping ActorSystem")
    await(system.terminate)
    logger.info("Stopped ActorSystem")

  }

  behavior of "RawTransactionRpc"

  it should "be able to sign a raw transaction" in async {
    val addressInfo = await(client.validateAddress(await(client.getNewAddress)))

    val multisig = await(client
      .addMultiSigAddress(1, Vector(Left(addressInfo.pubkey.get))))

    val txid = await(TestUtil.fundBlockChainTransaction(client, multisig.address, Bitcoins(1.2)))

    val rawTx = await(client.getTransaction(txid))

    val tx = await(
      client.decodeRawTransaction(rawTx.hex))

    val output =
      tx.vout.find(output => output.value == Bitcoins(1.2)).get

    val input = TransactionInput(
      TransactionOutPoint(txid.flip, UInt32(output.n)),
      P2SHScriptSignature(multisig.redeemScript.hex),
      UInt32.max - UInt32.one)

    val newAddress = await(client.getNewAddress)
    val ctx = await(
      client
        .createRawTransaction(
          Vector(input),
          Map(newAddress -> Bitcoins(1.1))))

    val result = await(client
      .signRawTransaction(
        ctx,
        Vector(
          RpcOpts.SignRawTransactionOutputParameter(
            txid,
            output.n,
            ScriptPubKey.fromAsmHex(
              output.scriptPubKey.hex),
            Some(multisig.redeemScript),
            Bitcoins(1.2)))))

    assert(result.complete)
  }

  it should "be able to combine raw transacitons" in async {
    val address1Info = await(client.getNewAddress.flatMap(client.validateAddress))
    val address2Info = await(otherClient.getNewAddress.flatMap(client.validateAddress))

    val multisig = await(
      client
        .addMultiSigAddress(
          2,
          Vector(
            Left(address1Info.pubkey.get),
            Left(address2Info.pubkey.get))))

    await(otherClient
      .addMultiSigAddress(
        2,
        Vector(
          Left(address1Info.pubkey.get),
          Left(address2Info.pubkey.get))))
    await(client.validateAddress(multisig.address))

    val txid = await(TestUtil.fundBlockChainTransaction(
      client,
      multisig.address,
      Bitcoins(1.2)))

    val rawTx = await(
      client.getTransaction(txid))

    val tx = await(
      client.decodeRawTransaction(rawTx.hex))

    val output = tx.vout
      .find(_.value == Bitcoins(1.2))
      .get

    val input = TransactionInput(
      TransactionOutPoint(txid.flip, UInt32(output.n)),
      P2SHScriptSignature(multisig.redeemScript.hex),
      UInt32.max - UInt32.one)

    val address = await(client.getNewAddress)

    val ctx = await(otherClient
      .createRawTransaction(
        Vector(input),
        Map(address -> Bitcoins(1.1))))

    val partialTx1 = await(client
      .signRawTransaction(
        ctx,
        Vector(RpcOpts
          .SignRawTransactionOutputParameter(
            txid,
            output.n,
            ScriptPubKey.fromAsmHex(output.scriptPubKey.hex),
            Some(multisig.redeemScript),
            Bitcoins(1.2)))))
    assert(!partialTx1.complete)
    assert(partialTx1.hex != ctx)

    val partialTx2 = await(otherClient
      .signRawTransaction(
        ctx,
        Vector(
          RpcOpts.SignRawTransactionOutputParameter(
            txid,
            output.n,
            ScriptPubKey.fromAsmHex(output.scriptPubKey.hex),
            Some(multisig.redeemScript),
            Bitcoins(1.2)))))

    assert(!partialTx2.complete)
    assert(partialTx2.hex != ctx)

    val combinedTx = await(client
      .combineRawTransaction(
        Vector(
          partialTx1.hex,
          partialTx2.hex)))

    await(client.sendRawTransaction(combinedTx))
    succeed
  }
}
