package org.bitcoins.core.api.callback

/** A data structure to represent all callbacks for a specific module */
trait ModuleCallbacks[T <: ModuleCallbacks[T]] {
  def +(other: T): T
}
