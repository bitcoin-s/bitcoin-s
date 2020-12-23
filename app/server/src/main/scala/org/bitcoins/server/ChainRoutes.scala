package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import org.bitcoins.commons.jsonmodels.BitcoinSServerInfo
import org.bitcoins.commons.serializers.Picklers._
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.config.BitcoinNetwork
import scodec.bits.ByteVector
import ujson._

import scala.concurrent.Future
import scala.util.{Failure, Success}

case class ChainRoutes(chain: ChainApi, network: BitcoinNetwork)(implicit
    system: ActorSystem)
    extends ServerRoute {
  import system.dispatcher

  def handleCommand: PartialFunction[ServerCommand, StandardRoute] = {
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
      GetBlockHeader.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetBlockHeader(hash)) =>
          complete {
            chain.getHeader(hash).flatMap {
              case None => Future.successful(Server.httpSuccess(ujson.Null))
              case Some(header) =>
                chain.getNumberOfConfirmations(hash).map {
                  case None =>
                    throw new RuntimeException(
                      s"Got unconfirmed header, ${header.hashBE.hex}")
                  case Some(confs) =>
                    val chainworkStr = {
                      val bytes = ByteVector(header.chainWork.toByteArray)
                      val padded = if (bytes.length <= 32) {
                        bytes.padLeft(32)
                      } else bytes

                      padded.toHex
                    }

                    val json = Obj(
                      "raw" -> Str(header.blockHeader.hex),
                      "hash" -> Str(header.hashBE.hex),
                      "confirmations" -> Num(confs),
                      "height" -> Num(header.height),
                      "version" -> Num(header.version.toLong.toDouble),
                      "versionHex" -> Str(header.version.hex),
                      "merkleroot" -> Str(header.merkleRootHashBE.hex),
                      "time" -> Num(header.time.toBigInt.toDouble),
                      "nonce" -> Num(header.nonce.toBigInt.toDouble),
                      "bits" -> Str(header.nBits.hex),
                      "difficulty" -> Num(header.difficulty.toDouble),
                      "chainwork" -> Str(chainworkStr),
                      "previousblockhash" -> Str(header.previousBlockHashBE.hex)
                    )

                    Server.httpSuccess(json)
                }
            }
          }
      }

    case ServerCommand("getinfo", _) =>
      complete {
        chain.getBestBlockHeader().map { header =>
          val info = BitcoinSServerInfo(network, header.height, header.hashBE)

          Server.httpSuccess(info.toJson)
        }
      }
  }

}
