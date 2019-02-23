package org.bitcoins.node.models

import slick.jdbc.PostgresProfile.api._
import slick.lifted.Tag

class PeerTable(tag: Tag) extends TableAutoInc[Peer](tag, "peer_table") {


  def * = ???
}
