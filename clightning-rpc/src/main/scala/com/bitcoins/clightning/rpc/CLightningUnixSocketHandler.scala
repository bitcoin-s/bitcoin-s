package com.bitcoins.clightning.rpc

import akka.http.scaladsl.model._
import akka.util._
import org.newsclub.net.unix.AFUNIXSocketAddress
import play.api.libs.json._

import java.io._
import java.util.UUID
import scala.concurrent._
import scala.util._

abstract class CLightningUnixSocketHandler { self: CLightningRpcClient =>

  def clightningCall[T](command: String, parameters: JsValue = JsArray.empty)(
      implicit reader: Reads[T]): Future[T] = {
    logger.trace(s"clightning rpc call $command")
    val responseF = sendRequest(command, parameters)

    responseF.map { payload =>
      parseResult(payload, command)
    }
  }

  private def parseResult[T](payload: RpcResult, commandName: String)(implicit
      reader: Reads[T]): T = {
    payload.result match {
      case Some(result) =>
        result.validate[T] match {
          case JsSuccess(value, _) => value
          case JsError(_) =>
            val errMsg =
              s"Could not parse JsResult for command: $commandName json: $result"
            logger.error(errMsg)
            throw new IllegalArgumentException(errMsg)
        }
      case None =>
        payload.error match {
          case Some(errJs) =>
            import RpcResult._
            errJs.validate[RpcError] match {
              case JsSuccess(err, _) =>
                val errMsg =
                  s"Error for command=$commandName, error code: ${err.code}, msg: ${err.message}"
                logger.error(errMsg)
                throw new RuntimeException(err.message)
              case JsError(_) =>
                throw new RuntimeException(
                  s"Could not parse error for command: $commandName, json: $errJs")
            }
          case None =>
            throw new RuntimeException(
              s"Did not get result or error for command: $commandName")
        }
    }
  }

  private def parseJson(response: String): RpcResult = {
    val json = Json.parse(response)

    json
      .validate[RpcResult]
      .getOrElse {
        val errMsg = s"Could not parse $json"
        logger.error(errMsg)
        throw new RuntimeException(errMsg)
      }
  }

  private def sendRequest(
      methodName: String,
      params: JsValue): Future[RpcResult] = {
    val uuid = UUID.randomUUID().toString
    val map: Map[String, JsValue] = Map("jsonrpc" -> JsString("2.0"),
                                        "id" -> JsString(uuid),
                                        "method" -> JsString(methodName),
                                        "params" -> params)

    val request: ByteString =
      HttpEntity(ContentTypes.`application/json`, JsObject(map).toString()).data

    Future.fromTry(doRawCall(request, uuid))
  }

  // -- below cribbed from https://github.com/clightning4j/JRPClightning/blob/7e1708d7ed67511ab5e67e9f4bc103d6b2c5be32/src/main/java/jrpc/service/socket/UnixDomainSocketRpc.java
  // todo make this more scala-y
  import org.newsclub.net.unix.AFUNIXSocket

  import java.net.Socket
  import java.nio.charset.StandardCharsets

  private def doRawCall(message: ByteString, uuid: String): Try[RpcResult] = {
    val socket = AFUNIXSocket.newInstance()
    socket.connect(AFUNIXSocketAddress.of(instance.rpcFile.toPath))
    val outputStream = socket.getOutputStream
    val callT = Try {
      // Send the message
      outputStream.write(message.toVector.toArray)
      outputStream.flush()
      // receive the message
      readAll(socket, uuid)
    }

    // close socket
    outputStream.close()
    socket.close()

    callT
  }

  private def readAll(socket: Socket, uuid: String): RpcResult = {
    val inputStream = socket.getInputStream
    val inputReader = new InputStreamReader(inputStream, StandardCharsets.UTF_8)
    val bufferedReader = new BufferedReader(inputReader)
    val stream = bufferedReader.lines()

    stream
      .filter { str =>
        logger.trace(s"Received response: $str")
        Try(parseJson(str).id == uuid).isSuccess
      }
      .map[RpcResult] { str =>
        parseJson(str)
      }
      .findFirst()
      .orElseThrow { () =>
        new RuntimeException(s"Could not find result for call $uuid")
      }
  }
}

private[clightning] case class RpcResult(
    jsonrpc: String,
    id: String,
    result: Option[JsValue],
    error: Option[JsValue])

private[clightning] case class RpcError(code: Long, message: String)

private[clightning] object RpcResult {
  implicit val rpcResultReads: Reads[RpcResult] = Json.reads[RpcResult]
  implicit val rpcErrorReads: Reads[RpcError] = Json.reads[RpcError]

}
