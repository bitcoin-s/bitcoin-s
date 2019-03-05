package org.bitcoins.db

/** This is meant to be coupled with [[CRUDAutoInc]]
  * and [[TableAutoInc]] to allow for automatically incrementing an id
  * when inserting something into a database. This removes the boiler
  * boiler plate from this having to happen every where a [[CRUD]]
  * is created
  */
abstract class DbRowAutoInc[T] {

  def id: Option[Long]

  def copyWithId(id: Long): T

}
