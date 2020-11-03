package org.bitcoins.feeprovider

import akka.actor.ActorSystem
import org.bitcoins.core.api.feeprovider.FeeRateApi

trait FeeProviderFactory[T <: FeeRateApi] {
  def fromBlockTarget(blocks: Int)(implicit system: ActorSystem): T
}
