package org.bitcoins.core.util

import org.bitcoins.core.util.sorted.OrderedNonces
import org.bitcoins.crypto.SchnorrNonce
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class OrderedNoncesTest extends BitcoinSUnitTest {

  behavior of "OrderedNonces"

  val unsorted = Vector(
    SchnorrNonce(
      "c4b89873c8753de3f0a9e94c4a6190badaa983513a6624a3469eb4577904bfea"),
    SchnorrNonce(
      "92efe81609c773d97da2b084eb691f48ef5e926acc6eecd629f80fb1184711bc")
  )

  it must "throw an exception if you create an unordered nonces" in {

    intercept[IllegalArgumentException] {
      OrderedNonces(unsorted)
    }
  }

  it must "sort nonces with OrderedNonces.fromUnsorted" in {
    val sorted = OrderedNonces.fromUnsorted(unsorted)
    assert(sorted.toVector != unsorted)
    assert(sorted.toVector == Vector(unsorted(1), unsorted(0)))
  }

  it must "sort these nonces" in {
    val nonces = Vector(
      "2aded38cfeb4709631822636e6df6269af17d202e8635614e4b1ee9e793a8caa",
      "66cb5eb5a52c901d99a16435d6e2a8a55c3b8a64f1a5474eb7fe071bd1dad2e0",
      "6e722e1093e8e120f3b2857604b1d56e0b95edb4734996fcd3754254cc855ffc",
      "895058a9be520c417bf85924b9801d6906bee76e2d21d54baf2ebce23b0a0959",
      "8c3fe8c069225798f4294b681b4ae76aecbae8d225b826c68153335395aae424",
      "9c4b3b0777409717af864f79960365a3290f2aff0e21ceb690551d9153daee95",
      "ac18ee79f2820bb32cacdd866fc2a38ca5d8004cf57b402b22892e7eea28b8ab",
      "ac584fb0cf7cfa2d1fdc4b50e28640d6c1a01ba6e0ffd4522236c1ec10959f68",
      "e16cf501e2692096cc3402eba1a010600e79889f60e3568ad1024e395b4f5378",
      "ff3b53425c80feb012b64fd834c27b07145e8bcd9078871ac19f6bf4b93f8d05"
    ).map(SchnorrNonce.fromHex)
    val sorted = OrderedNonces.fromUnsorted(nonces)

    sorted.foreach(println)

    succeed
  }
}
