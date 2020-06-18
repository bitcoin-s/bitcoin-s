package org.bitcoins.feeprovider

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import org.bitcoins.commons.jsonmodels.wallet.BitcoinerLiveResult
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import play.api.libs.json.{JsError, JsSuccess, Json}

import scala.util.{Failure, Success, Try}

case class BitcoinerLiveFeeRateProvider(minutes: Int)(implicit
    override val system: ActorSystem)
    extends CachedHttpFeeRateProvider {

  private val bitcoinerLiveValidMinutes =
    Vector(30, 60, 120, 180, 360, 720, 1440)
  require(
    bitcoinerLiveValidMinutes.contains(minutes),
    s"$minutes is not a valid selection, must be from $bitcoinerLiveValidMinutes")

  override val uri: Uri =
    Uri("https://bitcoiner.live/api/fees/estimates/latest")

  override def converter(str: String): Try[SatoshisPerVirtualByte] = {
    val json = Json.parse(str)
    json.validate[BitcoinerLiveResult] match {
      case JsSuccess(response, _) =>
        Success(response.estimates(minutes).sat_per_vbyte)
      case JsError(error) =>
        Failure(
          new RuntimeException(
            s"Unexpected error when parsing response: $error"))
    }
  }
}
