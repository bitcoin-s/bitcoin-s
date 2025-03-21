package org.bitcoins.server

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.Directives.complete
import org.apache.pekko.http.scaladsl.server.Route
import org.bitcoins.commons.jsonmodels.BitcoinSServerInfo
import org.bitcoins.commons.rpc.GetBlockHeader
import org.bitcoins.commons.serializers.Picklers
import org.bitcoins.commons.serializers.Picklers._
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.server.routes.{Server, ServerCommand, ServerRoute}
import org.bitcoins.server.util.ChainUtil

import scala.concurrent.Future

case class ChainRoutes(
    chain: ChainApi,
    network: BitcoinNetwork,
    startedTorConfigF: Future[Unit]
)(implicit system: ActorSystem)
    extends ServerRoute {
  import system.dispatcher

  def handleCommand: PartialFunction[ServerCommand, Route] = {
    case ServerCommand("getblockcount", _) =>
      complete {
        chain.getBlockCount().map { count =>
          Server.httpSuccess(count)
        }
      }
    case ServerCommand("getfiltercount", _) =>
      complete {
        chain.getFilterCount().map { count =>
          Server.httpSuccess(count)
        }
      }
    case ServerCommand("getfilterheadercount", _) =>
      complete {
        chain.getFilterHeaderCount().map { count =>
          Server.httpSuccess(count)
        }
      }
    case ServerCommand("getbestblockhash", _) =>
      complete {
        chain.getBestBlockHash().map { hash =>
          Server.httpSuccess(hash)
        }
      }

    case ServerCommand("getblockheader", arr) =>
      withValidServerCommand(GetBlockHeader.fromJsArr(arr)) {
        case GetBlockHeader(hash) =>
          complete {
            chain.getHeader(hash).flatMap {
              case None => Future.successful(Server.httpSuccess(ujson.Null))
              case Some(_) =>
                val resultsF =
                  ChainUtil.getBlockHeaderResult(Vector(hash), chain)
                for {
                  results <- resultsF
                } yield {
                  val json = upickle.default
                    .writeJs(results.head)(
                      using Picklers.getBlockHeaderResultPickler)
                  Server.httpSuccess(json)
                }
            }
          }
      }

    case ServerCommand("getinfo", _) =>
      complete {
        for {
          header <- chain.getBestBlockHeader()
          syncing <- chain.isSyncing()
          isIBD <- chain.isIBD()
        } yield {
          val info = BitcoinSServerInfo(
            network = network,
            blockHeight = header.height,
            blockHash = header.hashBE,
            torStarted = startedTorConfigF.isCompleted,
            syncing = syncing,
            isInitialBlockDownload = isIBD
          )
          Server.httpSuccess(info.toJson)
        }
      }

    case ServerCommand("getmediantimepast", _) =>
      complete {
        chain.getMedianTimePast().map { mtp =>
          Server.httpSuccess(mtp)
        }
      }
  }

}
