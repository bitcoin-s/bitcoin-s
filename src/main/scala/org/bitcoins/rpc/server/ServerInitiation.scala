package org.bitcoins.protocol.server

import sys.process._

/**
 * Created by Tom on 1/15/2016.
 */

class ServerInitiation(server : String) {


  val $RPC_USER = System.getenv("RPC_USER")
  val $RPC_PASS = System.getenv("RPC_PASS")
  Process(server).run

}
