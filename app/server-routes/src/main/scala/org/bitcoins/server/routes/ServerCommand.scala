package org.bitcoins.server.routes

import ujson._
import upickle.default._

// TODO ID?
case class ServerCommand(method: String, params: ujson.Arr)

object ServerCommand {

  implicit val rw: ReadWriter[ServerCommand] =
    readwriter[ujson.Value].bimap[ServerCommand](
      cmd => {
        if (cmd.params.arr.isEmpty)
          Obj("method" -> Str(cmd.method))
        else Obj("method" -> Str(cmd.method), "params" -> cmd.params)
      },
      json => {
        val obj = json.obj
        val method = obj("method").str
        if (obj.contains("params"))
          ServerCommand(method, obj("params").arr)
        else ServerCommand(method, Arr())
      }
    )
}
