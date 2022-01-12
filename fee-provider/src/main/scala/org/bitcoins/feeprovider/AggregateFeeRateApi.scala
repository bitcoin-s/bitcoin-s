package org.bitcoins.feeprovider

import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.FeeUnit

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

case class AggregateFeeRateApi(providers: Vector[FeeRateApi])(implicit
    ec: ExecutionContext)
    extends FeeRateApi {

  override def getFeeRate(): Future[FeeUnit] = {
    val init: Option[FeeUnit] = None
    val retOptF = FutureUtil.foldLeftAsync(init, providers) {
      case (ret, provider) =>
        ret match {
          case Some(value) => Future.successful(value).map(Some(_))
          case None =>
            provider
              .getFeeRate()
              .map(Some(_))
              .recover { case NonFatal(_) => None }
        }
    }

    retOptF.map {
      case Some(ret) => ret
      case None =>
        sys.error("Failed to get fee rate from any provider")
    }
  }
}
