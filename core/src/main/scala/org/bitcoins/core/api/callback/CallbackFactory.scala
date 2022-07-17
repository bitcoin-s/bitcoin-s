package org.bitcoins.core.api.callback

trait CallbackFactory[T] {
  def empty: T
}
