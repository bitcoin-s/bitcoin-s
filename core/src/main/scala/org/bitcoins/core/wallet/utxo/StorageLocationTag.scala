package org.bitcoins.core.wallet.utxo

sealed abstract class StorageLocationTag extends AddressTag {
  val typeName: String = "StorageLocationTag"
}

object StorageLocationTag extends AddressTagFactory[StorageLocationTag] {

  /** Keys stored on a computer connected to the internet */
  final case object HotStorage extends StorageLocationTag

  /** Keys stored on a hardware wallet or other offline device */
  final case object ColdStorage extends StorageLocationTag

  /** Keys stored on a hardware wallet or other offline device locked in a safe in a distant location */
  final case object DeepColdStorage extends StorageLocationTag

  val all: Vector[StorageLocationTag] =
    Vector(HotStorage, ColdStorage, DeepColdStorage)
}
