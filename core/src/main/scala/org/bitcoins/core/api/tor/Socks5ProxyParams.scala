package org.bitcoins.core.api.tor

import org.bitcoins.crypto.CryptoUtil

import java.net.InetSocketAddress

case class Socks5ProxyParams(
    address: InetSocketAddress,
    credentialsOpt: Option[Credentials],
    randomizeCredentials: Boolean
)

object Socks5ProxyParams {

  val DefaultPort = 9050

  val defaultProxyParams: Socks5ProxyParams =
    new Socks5ProxyParams(
      address = InetSocketAddress.createUnresolved("127.0.0.1", DefaultPort),
      credentialsOpt = None,
      randomizeCredentials = true
    )

  def proxyCredentials(proxyParams: Socks5ProxyParams): Option[Credentials] =
    if (proxyParams.randomizeCredentials) {
      // randomize credentials for every proxy connection to enable Tor stream isolation
      Some(
        Credentials(
          CryptoUtil.randomBytes(16).toHex,
          CryptoUtil.randomBytes(16).toHex
        )
      )
    } else {
      proxyParams.credentialsOpt
    }
}
