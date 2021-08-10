package org.bitcoins.feeprovider

import akka.actor.ActorSystem
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.tor.Socks5ProxyParams

trait FeeProviderFactory[T <: FeeRateApi] {

  def fromBlockTarget(blocks: Int, proxyParams: Option[Socks5ProxyParams])(
      implicit system: ActorSystem): T
}
