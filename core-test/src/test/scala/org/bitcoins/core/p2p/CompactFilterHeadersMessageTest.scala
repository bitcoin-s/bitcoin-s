package org.bitcoins.core.p2p

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.gcs.{FilterHeader, FilterType}
import org.bitcoins.testkit.util.BitcoinSUnitTest

class CompactFilterHeadersMessageTest extends BitcoinSUnitTest {

  it must "extract headers from a message" in {
    val message = CompactFilterHeadersMessage(
      FilterType.Basic,
      stopHash = DoubleSha256Digest.fromHex(
        "9783ef3f32cdf84a66329c30717560e1be50e53743ef415f4c1a714a4f731831"),
      previousFilterHeader = DoubleSha256Digest.fromHex(
        "0000000000000000000000000000000000000000000000000000000000000000"),
      filterHashes = Vector(
        DoubleSha256Digest.fromHex(
          "1f30de30fabb7892d15eb985cc5d6c34c54a11b7e4c51f3da498f16255a27bb1"),
        DoubleSha256Digest.fromHex(
          "12dc878ad0dbf1570b7717916b69a3c596d2f44c3648abb205d01498076e3dc7"),
        DoubleSha256Digest.fromHex(
          "3e42e4309701bbcfa1173294c2068872557191c4197b0b6db17ad07455fd0ad2"),
        DoubleSha256Digest.fromHex(
          "3632ce0c0243d3c857c06aa1514a4c55abd11c9a78a64b0ea5afe5e4454e762b"),
        DoubleSha256Digest.fromHex(
          "2780a5f5efd80f9cdb7db852b36cd8c7b10ea8af73b54875b8cb963e243ce779"),
        DoubleSha256Digest.fromHex(
          "0103dd9d2485c30e58b9c70649821de58c22ac145bb61cc43295e7b282c5e25e"),
        DoubleSha256Digest.fromHex(
          "f8c60959fc3d854e2c5e19abf8581ad945f085d6b3f33e34ea441b0f0fce247d"),
        DoubleSha256Digest.fromHex(
          "790a19612aaf4849261bc184a34cdd83bfd7dd2fd9fa6f2871f39cd7af4a97e4"),
        DoubleSha256Digest.fromHex(
          "01f1be7b43e18bac1cb57232169fc2985e47261b4b369ca42c3a9dae7deb2630"),
        DoubleSha256Digest.fromHex(
          "e9b74e7ca6ba7b4d83abd6406c7b1c0cd2f43e4f74140ac6498ae976d7b3b132"),
        DoubleSha256Digest.fromHex(
          "aeb7d751ef3bc6c65466d7962fa989ea519bd98759fdb68a7547ed70d3f7aa67")
      )
    )

    val headers = message.filterHeaders
    headers must be(
      Vector(
        FilterHeader(
          DoubleSha256Digest.fromHex(
            "1f30de30fabb7892d15eb985cc5d6c34c54a11b7e4c51f3da498f16255a27bb1"),
          DoubleSha256Digest.fromHex(
            "0000000000000000000000000000000000000000000000000000000000000000")
        ),
        FilterHeader(
          DoubleSha256Digest.fromHex(
            "12dc878ad0dbf1570b7717916b69a3c596d2f44c3648abb205d01498076e3dc7"),
          DoubleSha256Digest.fromHex(
            "2b5adc66021d5c775f630efd91518cf6ce3e9f525bbf54d9f0d709451e305e48")
        ),
        FilterHeader(
          DoubleSha256Digest.fromHex(
            "3e42e4309701bbcfa1173294c2068872557191c4197b0b6db17ad07455fd0ad2"),
          DoubleSha256Digest.fromHex(
            "3d21cd4e0c683629deee1846154934beb989f671480f97329a12521e49708662")
        ),
        FilterHeader(
          DoubleSha256Digest.fromHex(
            "3632ce0c0243d3c857c06aa1514a4c55abd11c9a78a64b0ea5afe5e4454e762b"),
          DoubleSha256Digest.fromHex(
            "e236b45099e4fa8007b828fd381a44d6fe30b5fadc116dccdc3cb3055a93982d")
        ),
        FilterHeader(
          DoubleSha256Digest.fromHex(
            "2780a5f5efd80f9cdb7db852b36cd8c7b10ea8af73b54875b8cb963e243ce779"),
          DoubleSha256Digest.fromHex(
            "49b5e5af58bd0c7443bda36a5d9870321699ee01df978d16d7475606d7ff62c5")
        ),
        FilterHeader(
          DoubleSha256Digest.fromHex(
            "0103dd9d2485c30e58b9c70649821de58c22ac145bb61cc43295e7b282c5e25e"),
          DoubleSha256Digest.fromHex(
            "ee8a1b0b427e5e66eb04c4e7aab94f15e7db553df16f88a0d9a6048d4726cf6d")
        ),
        FilterHeader(
          DoubleSha256Digest.fromHex(
            "f8c60959fc3d854e2c5e19abf8581ad945f085d6b3f33e34ea441b0f0fce247d"),
          DoubleSha256Digest.fromHex(
            "5eda6f75e5f83d686a4915b65501aceef999be16ea58fe51413f0cfb7dd6a54b")
        ),
        FilterHeader(
          DoubleSha256Digest.fromHex(
            "790a19612aaf4849261bc184a34cdd83bfd7dd2fd9fa6f2871f39cd7af4a97e4"),
          DoubleSha256Digest.fromHex(
            "add1a57d8150ecffba0f9fb4f2ee74652436a330112b30f8b33e2724c356e75f")
        ),
        FilterHeader(
          DoubleSha256Digest.fromHex(
            "01f1be7b43e18bac1cb57232169fc2985e47261b4b369ca42c3a9dae7deb2630"),
          DoubleSha256Digest.fromHex(
            "8dac8a8185608a0427f2709bd7e213a21d08c678ecfd18716572bf4e10d31ee5")
        ),
        FilterHeader(
          DoubleSha256Digest.fromHex(
            "e9b74e7ca6ba7b4d83abd6406c7b1c0cd2f43e4f74140ac6498ae976d7b3b132"),
          DoubleSha256Digest.fromHex(
            "b43e99adb112345e2f19ed481ea8cd7a8c089b4597fd1a623f49f65a57ddc2f0")
        ),
        FilterHeader(
          DoubleSha256Digest.fromHex(
            "aeb7d751ef3bc6c65466d7962fa989ea519bd98759fdb68a7547ed70d3f7aa67"),
          DoubleSha256Digest.fromHex(
            "76d1a761ca0e2337222487873c26757a225c597044041d1426006e948618df38")
        )
      ))

    val hashes = headers.map(_.filterHash)
    message.filterHashes must be(hashes)
  }

}
