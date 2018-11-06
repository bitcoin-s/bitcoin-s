package org.bitcoins.rpc.client

import akka.stream.ActorMaterializer
import play.api.libs.json._

import scala.concurrent.{ExecutionContext, Future}

trait BitcoindCall extends Client {


  protected def bitcoindCall[T](
    command: String,
    parameters: List[JsValue] = List.empty)(
    implicit reader: Reads[T],
    materializer: ActorMaterializer,
    executionContext: ExecutionContext): Future[T] = {
    val request = buildRequest(instance, command, JsArray(parameters))
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] = responseF.flatMap(getPayload)

    payloadF.map { payload =>
      parseResult((payload \ resultKey).validate[T], payload)
    }
  }

  // Should both logging and throwing be happening?
  private def parseResult[T](result: JsResult[T], json: JsValue): T = {
    checkUnitError[T](result, json)

    result match {
      case res: JsSuccess[T] => res.value
      case res: JsError =>
        (json \ errorKey).validate[RpcError] match {
          case err: JsSuccess[RpcError] =>
            logger.error(s"Error ${err.value.code}: ${err.value.message}")
            throw new RuntimeException(
              s"Error ${err.value.code}: ${err.value.message}")
          case _: JsError =>
            logger.error(JsError.toJson(res).toString())
            throw new IllegalArgumentException(
              s"Could not parse JsResult: ${(json \ resultKey).get}")
        }
    }
  }

  // Catches errors thrown by calls with Unit as the expected return type (which isn't handled by UnitReads)
  private def checkUnitError[T](result: JsResult[T], json: JsValue): Unit = {
    if (result == JsSuccess(())) {
      (json \ errorKey).validate[RpcError] match {
        case err: JsSuccess[RpcError] =>
          logger.error(s"Error ${err.value.code}: ${err.value.message}")
          throw new RuntimeException(
            s"Error ${err.value.code}: ${err.value.message}")
        case _: JsError =>
      }
    }
  }


}
