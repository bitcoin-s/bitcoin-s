package org.bitcoins.lnurl

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.client.RequestBuilding.Get
import akka.http.scaladsl.model.HttpRequest
import akka.util.ByteString
import grizzled.slf4j.Logging
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.ln.LnInvoice
import org.bitcoins.core.protocol.ln.currency._
import org.bitcoins.lnurl.json._
import org.bitcoins.lnurl.json.LnURLJsonModels._
import org.bitcoins.tor._
import play.api.libs.json._

import java.net.{URI, URL}
import scala.concurrent._

class LnURLClient(proxyParams: Option[Socks5ProxyParams])(implicit
    system: ActorSystem)
    extends Logging {
  implicit protected val ec: ExecutionContext = system.dispatcher

  private val http = Http(system)

  private def sendRequest(request: HttpRequest): Future[String] = {
    val httpConnectionPoolSettings =
      Socks5ClientTransport.createConnectionPoolSettings(
        new URI(request.uri.toString),
        proxyParams)

    http
      .singleRequest(request, settings = httpConnectionPoolSettings)
      .flatMap(response =>
        response.entity.dataBytes
          .runFold(ByteString.empty)(_ ++ _))
      .map(payload => payload.decodeString(ByteString.UTF_8))
  }

  private def sendRequestAndParse[T <: LnURLJsonModel](request: HttpRequest)(
      implicit reads: Reads[T]): Future[T] = {
    sendRequest(request)
      .map { str =>
        val json = Json.parse(str)
        json.validate[T] match {
          case JsSuccess(value, _) => value
          case JsError(errors) =>
            json.validate[LnURLStatus] match {
              case JsSuccess(value, _) =>
                throw new RuntimeException(
                  value.reason.getOrElse("Error parsing response"))
              case JsError(_) =>
                throw new RuntimeException(
                  s"Error parsing json $str, got ${errors.mkString("\n")}")
            }
        }
      }
  }

  def makeRequest(lnURL: LnURL): Future[LnURLResponse] = {
    makeRequest(lnURL.url)
  }

  def makeRequest(url: URL): Future[LnURLResponse] = {
    makeRequest(url.toString)
  }

  def makeRequest(str: String): Future[LnURLResponse] = {
    sendRequestAndParse[LnURLResponse](Get(str))
  }

  def getInvoice(
      pay: LnURLPayResponse,
      amount: LnCurrencyUnit): Future[LnInvoice] = {
    getInvoice(pay, amount.toSatoshis)
  }

  def getInvoice(
      pay: LnURLPayResponse,
      amount: MilliSatoshis): Future[LnInvoice] = {
    getInvoice(pay, amount.toSatoshis)
  }

  def getInvoice(
      pay: LnURLPayResponse,
      amount: CurrencyUnit): Future[LnInvoice] = {
    val msats = MilliSatoshis(amount)
    val symbol = if (pay.callback.toString.contains("?")) "&" else "?"
    val url = s"${pay.callback}${symbol}amount=${msats.toLong}"
    sendRequestAndParse[LnURLPayInvoice](Get(url)).map(_.pr)
  }

  def doWithdrawal(
      withdraw: LnURLWithdrawResponse,
      invoice: LnInvoice): Future[Boolean] = {
    val symbol = if (withdraw.callback.toString.contains("?")) "&" else "?"
    val url = s"${withdraw.callback}${symbol}k1=${withdraw.k1}&pr=$invoice"
    sendRequestAndParse[LnURLStatus](Get(url)).map(_.status.toUpperCase == "OK")
  }
}
