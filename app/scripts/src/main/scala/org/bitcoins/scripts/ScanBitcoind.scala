package org.bitcoins.scripts

import org.apache.pekko.NotUsed
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.scaladsl.{FileIO, Flow, Keep, Sink, Source}
import org.apache.pekko.stream.{ActorAttributes, IOResult, Supervision}
import org.apache.pekko.util.ByteString
import org.bitcoins.commons.serializers.JsonReaders
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.arithmetic.OP_NEGATE
import org.bitcoins.core.script.constant.{ScriptToken, _}
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.BitcoindRpcAppConfig
import org.bitcoins.server.routes.BitcoinSRunner
import org.bitcoins.server.util.BitcoinSAppScalaDaemon
import play.api.libs.json._

import java.nio.file.{Paths}
import java.time.{Duration, Instant}
import scala.concurrent.Future

/** Useful script for scanning bitcoind This file assumes you have
  * pre-configured the connection between bitcoin-s and bitcoind inside of
  * bitcoin-s.conf
  * @see
  *   https://bitcoin-s.org/docs/config/configuration#example-configuration-file
  */
class ScanBitcoind()(implicit
    override val system: ActorSystem,
    rpcAppConfig: BitcoindRpcAppConfig
) extends BitcoinSRunner[Unit] {

  implicit val tupleFormat: Format[(DoubleSha256DigestBE, String)] = {
    implicit val tupleFmt: Format[(DoubleSha256DigestBE, String)] =
      new Format[(DoubleSha256DigestBE, String)] {
        def writes(tuple: (DoubleSha256DigestBE, String)): JsValue = Json.obj(
          "txid" -> tuple._1.hex,
          "tx" -> tuple._2
        )

        def reads(json: JsValue): JsResult[(DoubleSha256DigestBE, String)] =
          for {
            digest <- (json \ "txid").validate[DoubleSha256DigestBE](
              using JsonReaders.DoubleSha256DigestBEReads)
            str <- (json \ "tx").validate[String]
          } yield (digest, str)
      }
    tupleFmt
  }

  override def start(): Future[Unit] = {

    val bitcoindF = rpcAppConfig.clientF
    val f = for {
      bitcoind <- bitcoindF
      _ <- countLargeInputOutputSize(bitcoind)
    } yield {
      ()
    }
    f.failed.foreach(err =>
      logger.error(s"Failed to count witness v1 mempool txs", err))
    f
  }

  override def stop(): Future[Unit] = {
    system
      .terminate()
      .map(_ => ())
  }

  val decider: Supervision.Decider = { exn =>
    logger.error(s"Stream exception", exn)
    Supervision.Resume
  }

  /** Searches a given Source[Int] that represents block heights applying f to
    * them and returning a Seq[T] with the results
    */
  def searchBlocks[T, Mat](
      bitcoind: BitcoindRpcClient,
      source: Source[Int, NotUsed],
      f: Block => T,
      sink: Sink[T, Future[Mat]],
      numParallelism: Int = Runtime.getRuntime.availableProcessors()
  ): Future[Mat] = {
    source
      .mapAsync(parallelism = numParallelism) { height =>
        bitcoind
          .getBlockHash(height)
          .flatMap(h => bitcoind.getBlockRaw(h))
          .map(b => (b, height))
      }
      .mapAsync(numParallelism) { case (block, height) =>
        logger.info(
          s"Searching block at height=$height hashBE=${block.blockHeader.hashBE.hex}"
        )
        FutureUtil.makeAsync { () =>
          f(block)
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(decider))
      .toMat(sink)(Keep.right)
      .run()
  }

  def countSegwitTxs(
      bitcoind: BitcoindRpcClient,
      startHeight: Int,
      endHeight: Int
  ): Future[Unit] = {
    val startTime = System.currentTimeMillis()
    val source: Source[Int, NotUsed] = Source(startHeight.to(endHeight))

    // in this simple example, we are going to count the number of witness transactions
    val countSegwitTxs: Block => Int = { (block: Block) =>
      block.transactions.count(_.isInstanceOf[WitnessTransaction])
    }
    val countsF: Future[Seq[Int]] = for {
      counts <- searchBlocks[Int, Seq[Int]](bitcoind,
                                            source,
                                            countSegwitTxs,
                                            Sink.seq[Int])
    } yield counts

    val countF: Future[Int] = countsF.map(_.sum)

    for {
      count <- countF
      endTime = System.currentTimeMillis()
      _ = logger.info(
        s"Count of segwit txs from height=${startHeight} to endHeight=${endHeight} is ${count}. It took ${endTime - startTime}ms "
      )
    } yield ()
  }

  case class InputOutputSize(
      txid: DoubleSha256DigestBE,
      inputCount: Int,
      outputCount: Int,
      outputAmounts: Vector[Long])
  import JsonReaders.DoubleSha256DigestBEReads
  import org.bitcoins.commons.serializers.JsonWriters.DoubleSha256DigestBEWrites
  implicit val inputOutputSizeReads: Reads[InputOutputSize] =
    Json.reads[InputOutputSize]
  implicit val inputOutputSizeWrites: Writes[InputOutputSize] =
    Json.writes[InputOutputSize]
  def countLargeInputOutputSize(bitcoind: BitcoindRpcClient): Future[Unit] = {
    val startTime = Instant.now()
    val startHeight = 0
    var counter = 0L
    val endHeightF = bitcoind.getBlockCount()
    val sourceF: Future[Source[Int, NotUsed]] = {
      endHeightF.map(endHeight => Source(startHeight.to(endHeight)))
    }

    // in this simple example, we are going to count the number of witness transactions
    val countLargeInputsOutputs: Block => Vector[InputOutputSize] = {
      (block: Block) =>
        block.transactions
          .filter { tx =>
            tx.inputs.size >= 64 || tx.outputs.size >= 64
          }
          .map { tx =>
            counter += 1
            InputOutputSize(tx.txIdBE,
                            tx.inputs.size,
                            tx.outputs.size,
                            tx.outputs.map(_.value.satoshis.toLong))
          }
    }
    val sink = Flow[Vector[InputOutputSize]]
      .mapConcat(identity)
      .map(inputOutputSizeWrites.writes)
      .map(b => Json.stringify(b) + "\n")
      .batch(50_000, { t => Vector(t) }) { (vec, t) => vec.appended(t) }
      .map(batch => ByteString(batch.mkString))
      .toMat(FileIO.toPath(Paths.get("large-input-outputs.json")))(Keep.right)
    val countsF: Future[IOResult] = for {
      source <- sourceF
      ioResult <- searchBlocks[Vector[InputOutputSize], IOResult](
        bitcoind,
        source,
        countLargeInputsOutputs,
        sink)
    } yield ioResult

    for {
      count <- countsF
      endTime = Instant.now()
      endHeight <- endHeightF
      _ = logger.info(
        s"Count of large input/output txs from height=${startHeight} to endHeight=${endHeight} is ${counter} $count. It took ${Duration
            .between(startTime, endTime)}"
      )
    } yield ()
  }

  private def isPrevOpPushData(
      asm: Seq[ScriptToken],
      currentIdx: Int): Boolean = {
    if (currentIdx > 0) {
      val prev = asm(currentIdx - 1)
      StackPushOperationFactory.pushDataOperations.contains(prev) || prev
        .isInstanceOf[BytesToPushOntoStack]
    } else {
      false
    }
  }

  private def existsNegative(
      asm: Seq[ScriptToken]): (Boolean, Vector[(ScriptToken, Long)]) = {
    val result: Vector[(Boolean, Vector[(ScriptToken, Long)])] =
      asm.zipWithIndex.toVector.map { case (token, idx) =>
        token match {
          case s: ScriptNumber =>
            if (s.toLong < 0 && !isPrevOpPushData(asm, idx)) {
              (true, Vector((s, s.toLong)))
            } else (false, Vector.empty)
          case const: ScriptConstant =>
            if (const.bytes.size < 5) {
              // largest number in the script is 4 bytes (OP_CLTV/OP_CSV input must be positive)
              if (const.toLong < 0 && !isPrevOpPushData(asm, idx)) {
                (true, Vector((const, const.toLong)))
              } else (false, Vector.empty)
            } else {
              (false, Vector.empty)
            }
          case op: ScriptOperation =>
            if (op == OP_NEGATE) {
              (true, Vector((op, op.toLong)))
            } else {
              (false, Vector.empty)
            }
        }
      }

    if (result.exists(_._1)) {
      val consolidate = result.filter(_._1).flatMap(_._2)
      (true, consolidate)
    } else {
      (false, Vector.empty)
    }
  }

  import ReadNegativeNumberResult.NegativeNumberResult
  def findNegativeNumbers(bitcoind: BitcoindRpcClient): Future[IOResult] = {
    val start = Instant.now()
    val startHeight = 0
    val endHeightF = bitcoind.getBlockCount()
    val sourceF: Future[Source[Int, NotUsed]] =
      endHeightF.map(h => Source(startHeight.to(h)))
    val find: Block => Vector[NegativeNumberResult] = { block =>
      block.transactions.flatMap { tx =>
        // search outputs and inputs for negative numbers
        val inputs: Vector[NegativeNumberResult] =
          tx.inputs.zipWithIndex
            .flatMap { case (i, idx) =>
              val scriptSig = ((i, idx), existsNegative(i.scriptSignature.asm))
              i.scriptSignature match {
                case p2sh: P2SHScriptSignature =>
                  val redeemScript =
                    ((i, idx), existsNegative(p2sh.redeemScript.asm))
                  Vector(scriptSig, redeemScript)
                case _: ScriptSignature => Vector.empty
              }
            }
            // filter out coinbase transaction's scriptSigs
            // due to branding in the coinbase tx
            .filter(_._2._1 && !tx.isCoinbase)
            .map { case ((i, idx), (_, negativeOps)) =>
              NegativeNumberResult(tx.txIdBE,
                                   idx,
                                   i.scriptSignature.asm.mkString(" "),
                                   negativeOps.mkString(" "),
                                   "scriptsig")
            }

        val outputs: Vector[NegativeNumberResult] =
          tx.outputs.zipWithIndex
            .map { case (o, idx) =>
              ((o, idx), existsNegative(o.scriptPubKey.asm))
            }
            .filter(tup =>
              tup._2._1 && !tup._1._1.scriptPubKey.asm.contains(OP_RETURN))
            .map { case ((o, idx), (_, negativeOps)) =>
              NegativeNumberResult(tx.txIdBE,
                                   idx,
                                   o.scriptPubKey.asm.mkString(" "),
                                   negativeOps.mkString(" "),
                                   "scriptpubkey")
            }
        val wit: Vector[NegativeNumberResult] = tx match {
          case wtx: WitnessTransaction =>
            wtx.witness.witnesses.zipWithIndex
              .flatMap { case (wit, idx) =>
                val consts = wit.stack.map(ScriptConstant.fromBytes)
                val stack = ((consts, idx), existsNegative(consts))
                val script = wit match {
                  case _: P2WPKHWitnessV0 => Vector.empty
                  case p2wsh: P2WSHWitnessV0 =>
                    Vector(((p2wsh.redeemScript.asm, idx),
                            existsNegative(p2wsh.redeemScript.asm)))
                  case _: TaprootKeyPath => Vector.empty
                  case tr: TaprootScriptPath =>
                    Vector(
                      ((tr.script.asm, idx), existsNegative(tr.script.asm)))
                  case EmptyScriptWitness    => Vector.empty
                  case _: TaprootUnknownPath => Vector.empty
                }

                Vector(stack) ++ script

              }
              .filter(_._2._1)
              .map { case ((wit, idx), (_, negativeOps)) =>
                NegativeNumberResult(tx.txIdBE,
                                     idx,
                                     wit.mkString(" "),
                                     negativeOps.mkString(" "),
                                     "witness")
              }
          case _: NonWitnessTransaction =>
            Vector.empty
        }
        inputs ++ outputs ++ wit
      }
    }

    for {
      source <- sourceF
      sink = Flow[Vector[NegativeNumberResult]]
        .mapConcat(identity)
        .map(Json.toJson(_)(
          using ReadNegativeNumberResult.negativeNumberResultWrites))
        .map(b => Json.stringify(b) + "\n")
        .batch(50_000, { t => Vector(t) }) { (vec, t) => vec.appended(t) }
        .map(batch => ByteString(batch.mkString))
        .toMat(FileIO.toPath(Paths.get("negative-number-txs.json")))(Keep.right)
      result <- searchBlocks(bitcoind, source, find, sink)
      endHeight <- endHeightF
    } yield {
      logger.info(
        s"Done searching blocks endHeight=$endHeight, it took=${Duration
            .between(start, Instant.now())}")
      result
    }
  }

  def countTaprootTxsInBlocks(
      endHeight: Int,
      lastBlocks: Int,
      bitcoind: BitcoindRpcClient
  ): Future[Int] = {
    val startTime = System.currentTimeMillis()
    val startHeight = endHeight - lastBlocks
    val source: Source[Int, NotUsed] = Source(startHeight.to(endHeight))
    val countTaprootOutputs: Block => Int = { block =>
      val outputs = block.transactions
        .flatMap(_.outputs)
        .filter(_.scriptPubKey.isInstanceOf[TaprootScriptPubKey])
      outputs.length
    }

    val countsF: Future[Seq[Int]] = for {
      counts <- searchBlocks[Int, Seq[Int]](bitcoind,
                                            source,
                                            countTaprootOutputs,
                                            Sink.seq)
    } yield counts

    val countF: Future[Int] = countsF.map(_.sum)

    for {
      count <- countF
      endTime = System.currentTimeMillis()
      _ = logger.info(
        s"Count of taproot outputs from height=${startHeight} to endHeight=${endHeight} is ${count}. It took ${endTime - startTime}ms "
      )
    } yield count
  }

  def countWitV1MempoolTxs(bitcoind: BitcoindRpcClient): Future[Int] = {
    val memPoolSourceF = getMemPoolSource(bitcoind)
    val countF = memPoolSourceF.flatMap(_.runFold(0) { case (count, tx) =>
      count + tx.outputs.count(_.scriptPubKey.isInstanceOf[TaprootScriptPubKey])
    })
    countF.foreach(c =>
      logger.info(
        s"Found $c mempool transactions with witness v1 outputs at ${Instant.now}"
      ))
    countF
  }

  def getMemPoolSource(
      bitcoind: BitcoindRpcClient
  ): Future[Source[Transaction, NotUsed]] = {
    val mempoolF = bitcoind.getRawMemPool().map(_.txids)
    val sourceF: Future[Source[DoubleSha256DigestBE, NotUsed]] =
      mempoolF.map(Source(_))

    val mempoolTxSourceF: Future[Source[Transaction, NotUsed]] = {
      sourceF.map { source =>
        source.mapAsync(Runtime.getRuntime.availableProcessors()) { hash =>
          bitcoind
            .getRawTransaction(hash)
            .map(_.hex)
        }
      }
    }

    mempoolTxSourceF
  }
}

object ScanBitcoind extends BitcoinSAppScalaDaemon {

  override val actorSystemName: String =
    s"scan-bitcoind-${System.currentTimeMillis()}"

  override val customFinalDirOpt = None

  implicit val rpcAppConfig: BitcoindRpcAppConfig =
    BitcoindRpcAppConfig.fromDefaultDatadir()(using system)

  new ScanBitcoind().run()
}
