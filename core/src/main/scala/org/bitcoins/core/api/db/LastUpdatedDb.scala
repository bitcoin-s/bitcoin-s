package org.bitcoins.core.api.db

import java.time.Instant

/** A db row that has a lastUpdated column */
abstract class LastUpdatedDb {

  /** The time this object as last updated in the database */
  def lastUpdated: Instant
}
