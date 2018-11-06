package org.bitcoins.rpc.client.common

import org.bitcoins.core.number.UInt32
import org.bitcoins.rpc.jsonmodels.GetMemoryInfoResult
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json.{JsArray, JsString, Json}

import scala.concurrent.Future

/**
  * RPC calls related to administration of a given node
  */
trait NodeRpc extends Client {

  def abortRescan(): Future[Unit] = {
    bitcoindCall[Unit]("abortrescan")
  }

  private def logging(
      include: Option[Vector[String]],
      exclude: Option[Vector[String]]): Future[Map[String, Int]] = {
    val params =
      if (include.isEmpty && exclude.isEmpty) {
        List.empty
      } else if (include.isEmpty) {
        List(JsArray.empty, Json.toJson(exclude.get))
      } else if (exclude.isEmpty) {
        List(Json.toJson(include.get), JsArray.empty)
      } else {
        List(Json.toJson(include.get), Json.toJson(exclude.get))
      }
    bitcoindCall[Map[String, Int]]("logging", params)
  }
  def logging: Future[Map[String, Int]] = logging(None, None)

  def logging(
      include: Vector[String] = Vector.empty,
      exclude: Vector[String] = Vector.empty): Future[Map[String, Int]] = {
    val inc = if (include.nonEmpty) Some(include) else None
    val exc = if (exclude.nonEmpty) Some(exclude) else None
    logging(inc, exc)
  }

  def uptime: Future[UInt32] = {
    bitcoindCall[UInt32]("uptime")
  }

  def getMemoryInfo: Future[GetMemoryInfoResult] = {
    bitcoindCall[GetMemoryInfoResult]("getmemoryinfo")
  }

  def help(rpcName: String = ""): Future[String] = {
    bitcoindCall[String]("help", List(JsString(rpcName)))
  }

  def stop(): Future[String] = {
    bitcoindCall[String]("stop")
  }
}
