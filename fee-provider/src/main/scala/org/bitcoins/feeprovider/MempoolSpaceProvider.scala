package org.bitcoins.feeprovider

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import org.bitcoins.commons.jsonmodels.wallet.MempoolSpaceResult
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.feeprovider.MempoolSpaceTarget._
import play.api.libs.json.{JsError, JsSuccess, Json}

import scala.util.{Failure, Success, Try}

/** Fetches fee rate from mempool.space's API
  * Documentation found here: https://mempool.space/about
  */
case class MempoolSpaceProvider(target: MempoolSpaceTarget)(implicit
    override val system: ActorSystem)
    extends CachedHttpFeeRateProvider {

  override val uri: Uri =
    Uri("https://mempool.space/api/v1/fees/recommended")

  override def converter(str: String): Try[SatoshisPerVirtualByte] = {
    val json = Json.parse(str)
    json.validate[MempoolSpaceResult] match {
      case JsSuccess(response, _) =>
        target match {
          case FastestFeeTarget =>
            Success(response.fastestFee)
          case HalfHourFeeTarget =>
            Success(response.halfHourFee)
          case HourFeeTarget =>
            Success(response.hourFee)
        }
      case JsError(error) =>
        Failure(
          new RuntimeException(
            s"Unexpected error when parsing response: $error"))
    }
  }
}

object MempoolSpaceProvider extends FeeProviderFactory[MempoolSpaceProvider] {

  override def fromBlockTarget(blocks: Int)(implicit
      system: ActorSystem): MempoolSpaceProvider = {
    val target = MempoolSpaceTarget.fromBlockTarget(blocks)
    MempoolSpaceProvider(target)
  }
}

abstract class MempoolSpaceTarget

object MempoolSpaceTarget {

  final case object FastestFeeTarget extends MempoolSpaceTarget

  final case object HalfHourFeeTarget extends MempoolSpaceTarget

  final case object HourFeeTarget extends MempoolSpaceTarget

  def fromBlockTarget(blocks: Int): MempoolSpaceTarget = {
    if (blocks <= 0) {
      throw new IllegalArgumentException(
        s"Cannot have a negative or zero block target, got $blocks")
    } else if (blocks < 3) {
      FastestFeeTarget
    } else if (blocks < 6) {
      HalfHourFeeTarget
    } else {
      HourFeeTarget
    }
  }
}
