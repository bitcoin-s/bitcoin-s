package org.bitcoins.feeprovider

import java.time.{Duration, Instant}
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.util.ByteString
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.tor.{Socks5ClientTransport, Socks5ProxyParams}

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.Try

object HttpFeeRateProvider {

  def makeApiCall(uri: Uri, proxyParam: Option[Socks5ProxyParams])(implicit
      system: ActorSystem): Future[String] = {
    implicit val ec: ExecutionContextExecutor = system.dispatcher
    val connectionPoolSettings =
      Socks5ClientTransport.createConnectionPoolSettings(proxyParam)
    Http(system)
      .singleRequest(HttpRequest(uri = uri), settings = connectionPoolSettings)
      .flatMap(response =>
        response.entity.dataBytes
          .runFold(ByteString.empty)(_ ++ _)
          .map(payload => payload.decodeString(ByteString.UTF_8)))
  }
}

abstract class HttpFeeRateProvider[T <: FeeUnit] extends FeeRateApi {
  implicit protected val system: ActorSystem

  protected def uri: Uri

  protected def converter(str: String): Try[T]

  protected def proxyParams: Option[Socks5ProxyParams]

  override def getFeeRate(): Future[T] = {
    HttpFeeRateProvider
      .makeApiCall(uri, proxyParams)
      .flatMap(ret => Future.fromTry(converter(ret)))(system.dispatcher)
  }
}

abstract class CachedHttpFeeRateProvider[T <: FeeUnit]
    extends HttpFeeRateProvider[T] {

  private var cachedFeeRateOpt: Option[(T, Instant)] = None

  val cacheDuration: Duration = Duration.ofMinutes(5)

  private def updateFeeRate(): Future[T] = {
    implicit val ec: ExecutionContextExecutor = system.dispatcher
    super.getFeeRate().map { feeRate =>
      cachedFeeRateOpt = Some((feeRate, TimeUtil.now))
      feeRate
    }
  }

  override def getFeeRate(): Future[T] = {
    cachedFeeRateOpt match {
      case None =>
        updateFeeRate()
      case Some((cachedFeeRate, time)) =>
        val now = TimeUtil.now
        val timeout = time.plus(cacheDuration)
        if (now.isAfter(timeout)) {
          updateFeeRate()
        } else {
          Future.successful(cachedFeeRate)
        }
    }
  }
}
