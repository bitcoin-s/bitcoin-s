package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.currency._
import org.bitcoins.wallet.api.UnlockedWalletApi
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.wallet.fee.SatoshisPerByte

case class WalletRoutes(wallet: UnlockedWalletApi)(implicit system: ActorSystem)
    extends BitcoinSLogger {
  import system.dispatcher
  implicit val materializer = ActorMaterializer()

  val routes: Route = {
    pathPrefix("wallet") {
      path("getbalance") {
        complete {
          wallet.getBalance().map { balance =>
            HttpEntity(
              ContentTypes.`application/json`,
              Bitcoins(balance.satoshis).toBigDecimal.toString()
            )
          }
        }
      } ~
        path("getnewaddress") {
          complete {
            wallet.getNewAddress().map { address =>
              HttpEntity(ContentTypes.`application/json`, address.value)
            }
          }
        } ~
        // TODO change this to be POST body parameters
        path("sendtoaddress" / Segment / LongNumber) {
          case (addrString, amountBitcoins) =>
            val address = BitcoinAddress.fromStringExn(addrString)
            val amount = Bitcoins(amountBitcoins)

            // TODO dynamic fees
            val feeRate = SatoshisPerByte(100.sats)
            complete {
              wallet.sendToAddress(address, amount, feeRate).map { tx =>
                // TODO this TX isn't being broadcast anywhere
                // would be better to dump the entire TX hex until that's implemented?
                HttpEntity(ContentTypes.`application/json`, tx.txIdBE.hex)
              }
            }
        }
    }
  }
}
