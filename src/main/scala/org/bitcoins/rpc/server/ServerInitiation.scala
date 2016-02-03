package org.bitcoins.protocol.server

import sys.process._

/**
 * Created by Tom on 1/15/2016.
 */
class ServerInitiation(server : String) {

  Process(server).run

}
