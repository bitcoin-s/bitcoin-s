package org.bitcoins.esplora

import akka.actor.ActorSystem
import akka.http.scaladsl._
import akka.http.scaladsl.client.RequestBuilding.Post
import akka.http.scaladsl.model._
import akka.util.ByteString
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.crypto._
import org.bitcoins.esplora.EsploraJsonModels._
import org.bitcoins.tor._
import play.api.libs.json._
import scodec.bits.ByteVector

import java.net.URI
import scala.concurrent._
import scala.util.control.NonFatal

class EsploraClient(site: EsploraSite, proxyParams: Option[Socks5ProxyParams])(
    implicit system: ActorSystem)
    extends ChainQueryApi {
  implicit val ec: ExecutionContext = system.dispatcher

  require(!site.isTor || site.isTor && proxyParams.isDefined,
          "proxyParams must be defined for a tor esplora site")

  private val baseUrl = site.url

  private val httpClient: HttpExt = Http(system)

  private val httpConnectionPoolSettings =
    Socks5ClientTransport.createConnectionPoolSettings(new URI(baseUrl),
                                                       proxyParams)

  private def sendRawRequest(request: HttpRequest): Future[ByteString] = {
    httpClient
      .singleRequest(request, settings = httpConnectionPoolSettings)
      .flatMap(response =>
        response.entity.dataBytes
          .runFold(ByteString.empty)(_ ++ _))
  }

  private def sendRequest(request: HttpRequest): Future[String] = {
    sendRawRequest(request)
      .map(payload => payload.decodeString(ByteString.UTF_8))
  }

  private def sendRequestAndParse[T](httpRequest: HttpRequest)(implicit
      reads: Reads[T]): Future[T] = {
    sendRequest(httpRequest).map { str =>
      val json = Json.parse(str)
      json.validate[T] match {
        case JsSuccess(value, _) => value
        case JsError(errors) =>
          throw new RuntimeException(
            s"Error parsing json $json, ${errors.mkString("\n")}")
      }
    }
  }

  def getTransaction(txId: DoubleSha256DigestBE): Future[EsploraTransaction] = {
    val url = baseUrl + s"/tx/${txId.hex}"
    val request = HttpRequest(uri = url, method = HttpMethods.GET)

    sendRequestAndParse[EsploraTransaction](request)
  }

  def getTransactionStatus(
      txId: DoubleSha256DigestBE): Future[EsploraTransactionStatus] = {
    val url = baseUrl + s"/tx/${txId.hex}/status"
    val request = HttpRequest(uri = url, method = HttpMethods.GET)

    sendRequestAndParse[EsploraTransactionStatus](request)
  }

  def getRawTransaction(txId: DoubleSha256DigestBE): Future[Transaction] = {
    val url = baseUrl + s"/tx/${txId.hex}/raw"
    val request = HttpRequest(uri = url, method = HttpMethods.GET)

    sendRawRequest(request).map { byteString =>
      val bytes = ByteVector(byteString.toArray)
      Transaction.fromBytes(bytes)
    }
  }

  def broadcastTransaction(tx: Transaction): Future[DoubleSha256DigestBE] = {
    val url = baseUrl + "/tx"
    val request = Post(url, tx.hex)

    sendRequest(request).map(DoubleSha256DigestBE.fromHex)
  }

  def getAddressStats(addr: BitcoinAddress): Future[AddressStats] = {
    val url = baseUrl + s"/address/$addr"
    val request = HttpRequest(uri = url, method = HttpMethods.GET)

    sendRequestAndParse[AddressStats](request)
  }

  def getAddressTxs(
      addr: BitcoinAddress): Future[Vector[EsploraTransaction]] = {
    val url = baseUrl + s"/address/$addr/txs"
    val request = HttpRequest(uri = url, method = HttpMethods.GET)

    sendRequestAndParse[Vector[EsploraTransaction]](request)
  }

  def getAddressTxs(
      addr: BitcoinAddress,
      lastSeenTxId: DoubleSha256DigestBE): Future[
    Vector[EsploraTransaction]] = {
    val url = baseUrl + s"/address/$addr/txs/chain/${lastSeenTxId.hex}"
    val request = HttpRequest(uri = url, method = HttpMethods.GET)

    sendRequestAndParse[Vector[EsploraTransaction]](request)
  }

  def getMempoolTxs(
      addr: BitcoinAddress): Future[Vector[EsploraTransaction]] = {
    val url = baseUrl + s"/address/$addr/txs/mempool"
    val request = HttpRequest(uri = url, method = HttpMethods.GET)

    sendRequestAndParse[Vector[EsploraTransaction]](request)
  }

  def getBlock(hash: DoubleSha256DigestBE): Future[EsploraBlock] = {
    val url = baseUrl + s"/block/${hash.hex}"
    val request = HttpRequest(uri = url, method = HttpMethods.GET)

    sendRequestAndParse[EsploraBlock](request)
  }

  def getBlockHeader(hash: DoubleSha256DigestBE): Future[BlockHeader] = {
    val url = baseUrl + s"/block/${hash.hex}/header"
    val request = HttpRequest(uri = url, method = HttpMethods.GET)

    sendRequest(request).map(BlockHeader.fromHex)
  }

  def getBlockHashAtHeight(height: Int): Future[DoubleSha256DigestBE] = {
    val url = baseUrl + s"/block-height/$height"
    val request = HttpRequest(uri = url, method = HttpMethods.GET)

    sendRequest(request).map(DoubleSha256DigestBE.fromHex)
  }

  def getRawBlock(hash: DoubleSha256DigestBE): Future[Block] = {
    val url = baseUrl + s"/block/${hash.hex}/raw"
    val request = HttpRequest(uri = url, method = HttpMethods.GET)

    sendRawRequest(request).map { byteString =>
      val bytes = ByteVector(byteString.toArray)
      Block.fromBytes(bytes)
    }
  }

  // -- ChainQueryApi --
  override def getBestHashBlockHeight()(implicit
      _ec: ExecutionContext): Future[Int] = {
    val url = baseUrl + s"/blocks/tip/height"
    val request = HttpRequest(uri = url, method = HttpMethods.GET)

    sendRequest(request).map(_.toInt)(ec)
  }

  override def getBestBlockHash(): Future[DoubleSha256DigestBE] = {
    val url = baseUrl + s"/blocks/tip/hash"
    val request = HttpRequest(uri = url, method = HttpMethods.GET)

    sendRequest(request).map(DoubleSha256DigestBE.fromHex)
  }

  override def getBlockHeight(
      blockHash: DoubleSha256DigestBE): Future[Option[Int]] = {
    getBlock(blockHash)
      .map(_.height)
      .map(Some(_))
      .recover { case NonFatal(_) => None }
  }

  override def getNumberOfConfirmations(
      blockHash: DoubleSha256DigestBE): Future[Option[Int]] = {
    val tipF = getBestHashBlockHeight()
    val heightF = getBlockHeight(blockHash)

    for {
      tip <- tipF
      heightOpt <- heightF
    } yield heightOpt.map(height => tip - height + 1)
  }

  override def getFilterCount(): Future[Int] = Future.failed(
    new UnsupportedOperationException("Esplora does not support block filters"))

  override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] = {
    blockStamp match {
      case blockHeight: BlockStamp.BlockHeight =>
        Future.successful(blockHeight.height)
      case blockHash: BlockStamp.BlockHash =>
        getBlock(blockHash.hash).map(_.height)
      case blockTime: BlockStamp.BlockTime =>
        Future.failed(
          new UnsupportedOperationException(
            s"Not implemented for Esplora Client: $blockTime"))
    }
  }

  override def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int): Future[Vector[ChainQueryApi.FilterResponse]] =
    Future.failed(
      new UnsupportedOperationException(
        "Esplora does not support block filters"))

  override def epochSecondToBlockHeight(time: Long): Future[Int] =
    Future.successful(0)

  override def getMedianTimePast(): Future[Long] = {
    for {
      hash <- getBestBlockHash()
      block <- getBlock(hash)
    } yield block.mediantime.toLong
  }
}
