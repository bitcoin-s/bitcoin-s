package org.bitcoins.tor

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.settings.{
  ClientConnectionSettings,
  ConnectionPoolSettings
}
import akka.util.ByteString
import org.bitcoins.testkit.tor.CachedTor
import org.bitcoins.testkit.util.{BitcoinSAsyncTest, TorUtil}

class Socks5ClientTransportSpec extends BitcoinSAsyncTest with CachedTor {

  implicit val ec = system.dispatcher

  val proxyParams = torConfig.socks5ProxyParams.get

  val socks5ClientTransport = new Socks5ClientTransport(proxyParams)

  val clientConnectionSettings =
    ClientConnectionSettings(system).withTransport(socks5ClientTransport)

  val settings = ConnectionPoolSettings(system).withConnectionSettings(
    clientConnectionSettings)

  it should "handle clear net addresses" in {
    assume(TorUtil.torEnabled, "TOR environment variable is not set")
    for {
      response <- Http().singleRequest(
        HttpRequest(uri = "http://blockstream.info/"),
        settings = settings)
    } yield {
      // should redirect to the HTTPS endpoint
      assert(response.status.intValue() == 301)
      assert(
        response.headers
          .find(_.lowercaseName() == "location")
          .map(_.value())
          .contains("https://blockstream.info/"))
    }
  }

  it should "handle onion addresses" in {
    assume(TorUtil.torEnabled, "TOR environment variable is not set")
    for {
      response <- Http().singleRequest(
        HttpRequest(uri =
          "http://explorerzydxu5ecjrkwceayqybizmpjjznk5izmitf2modhcusuqlid.onion/"),
        settings = settings)
      html <- response.entity.dataBytes
        .runFold(ByteString(""))(_ ++ _)
        .map(x => x.utf8String)
    } yield {
      assert(response.status.intValue() == 200)
      assert(html.contains("Bitcoin Explorer"))
    }
  }

}
