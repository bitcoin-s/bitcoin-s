package org.bitcoins.feeprovider

import akka.actor.ActorSystem
import grizzled.slf4j.Logging
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.feeprovider.FeeProviderName.{
  BitGo,
  BitcoinerLive,
  Constant,
  MempoolSpace
}
import org.bitcoins.feeprovider.MempoolSpaceTarget.HourFeeTarget
import org.bitcoins.tor.Socks5ProxyParams

trait FeeProviderFactory[T <: FeeRateApi] {

  def fromBlockTarget(blocks: Int, proxyParams: Option[Socks5ProxyParams])(
      implicit system: ActorSystem): T
}

object FeeProviderFactory extends Logging {

  /** Gets a Fee Provider from the given wallet app config
    * Returns default if there is no config set
    */
  def getFeeProviderOrElse(
      default: => FeeRateApi,
      feeProviderNameStrOpt: Option[String],
      feeProviderTargetOpt: Option[Int],
      proxyParamsOpt: Option[Socks5ProxyParams],
      network: BitcoinNetwork)(implicit system: ActorSystem): FeeRateApi = {

    val feeProviderNameOpt =
      feeProviderNameStrOpt.flatMap(FeeProviderName.fromStringOpt)

    val feeProvider =
      (feeProviderNameOpt, feeProviderTargetOpt) match {
        case (None, None) | (None, Some(_)) =>
          default
        case (Some(BitcoinerLive), None) =>
          BitcoinerLiveFeeRateProvider.fromBlockTarget(6, proxyParamsOpt)
        case (Some(BitcoinerLive), Some(target)) =>
          BitcoinerLiveFeeRateProvider.fromBlockTarget(target, proxyParamsOpt)
        case (Some(BitGo), targetOpt) =>
          BitGoFeeRateProvider(targetOpt, proxyParamsOpt)
        case (Some(MempoolSpace), None) =>
          MempoolSpaceProvider(HourFeeTarget, network, proxyParamsOpt)
        case (Some(MempoolSpace), Some(target)) =>
          MempoolSpaceProvider.fromBlockTarget(target, network, proxyParamsOpt)
        case (Some(Constant), Some(num)) =>
          ConstantFeeRateProvider(SatoshisPerVirtualByte.fromLong(num))
        case (Some(Constant), None) =>
          throw new IllegalArgumentException(
            "Missing a target for a ConstantFeeRateProvider")
      }

    logger.info(s"Using fee provider: $feeProvider")
    feeProvider
  }
}
