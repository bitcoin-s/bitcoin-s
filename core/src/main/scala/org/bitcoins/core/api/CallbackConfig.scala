package org.bitcoins.core.api

trait CallbackConfig[T] {
  def addCallbacks(newCallbacks: T): T

  def callBacks: T
}
