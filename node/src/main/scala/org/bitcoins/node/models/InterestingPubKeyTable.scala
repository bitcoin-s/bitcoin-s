package org.bitcoins.node.models

import slick.jdbc.SQLiteProfile.api._
import org.bitcoins.core.crypto.ECPublicKey
import slick.lifted.ProvenShape

class InterestingPubKeyTable(tag: Tag)
    extends Table[ECPublicKey](tag, "interesting_pubkeys") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  val pubkey = column[ECPublicKey]("pubkey")

  def * : ProvenShape[ECPublicKey] =
    pubkey <> ((pub => pub), ((pub: ECPublicKey) => Some(pub)))
}
