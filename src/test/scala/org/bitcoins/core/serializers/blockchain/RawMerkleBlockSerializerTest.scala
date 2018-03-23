package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.{ BlockHeader, MerkleBlock, PartialMerkleTree }
import org.bitcoins.core.util.{ BitcoinSUtil, Leaf, Node }
import org.scalatest.{ FlatSpec, MustMatchers }

/**
 * Created by chris on 8/22/16.
 */
class RawMerkleBlockSerializerTest extends FlatSpec with MustMatchers {

  "RawMerkleBlockSerializer" must "serialize a merkle block generated inside of scalacheck" in {

    val (merkleBlock, txIds) = (MerkleBlock(BlockHeader(
      UInt32(49150652),
      DoubleSha256Digest("6cf34aac6e3de2bf4b429d114ed4572a7ce4b1c44f2091ae6825ee9774dbae2f"),
      DoubleSha256Digest("4487def8ba376b38c1e4e5910d3c9efd27e740cb9be8d452598cbf2e243fad8a"),
      UInt32(2941790316L), UInt32(1626267458), UInt32(1688549344)), UInt32(1),
      PartialMerkleTree(
        Leaf(DoubleSha256Digest(
        "442abdc8e74ad35ebd9571f88fda91ff511dcda8d241a5aed52cea1e00d69e03")),
        UInt32(1),
        List(false, false, false, false, false, false, false, false),
        List(DoubleSha256Digest("442abdc8e74ad35ebd9571f88fda91ff511dcda8d241a5aed52cea1e00d69e03")))), List())

    val hex = "bcfaed026cf34aac6e3de2bf4b429d114ed4572a7ce4b1c44f2091ae6825ee9774dbae2f4487def8ba376b38c1e4e5910d3c9efd27e740cb9be8d452598cbf2e243fad8a6c2858af42dfee60e037a5640100000001442abdc8e74ad35ebd9571f88fda91ff511dcda8d241a5aed52cea1e00d69e030100"
    val actualMerkleBlock = MerkleBlock(hex)
    actualMerkleBlock must equal(merkleBlock)
  }

  it must "not have any extra hashes left over when deserializing a previously valid partial merkle tree" in {
    val (merkleBlock, txIds) = (
      MerkleBlock(BlockHeader(
      UInt32(1626792925),
      DoubleSha256Digest("de2fc5fac498126f27c8adaa17aa86a1ef15d2b0adf5f2d2c056495bec17153f"),
      DoubleSha256Digest("f27404d701b9047cfcaa8d8454d2ecc12f4aa3e900ba8e5945bbb9289d67dd63"),
      UInt32(3098237133L), UInt32(359220269), UInt32(590323230)), UInt32(6),
      PartialMerkleTree(
        Node(
        DoubleSha256Digest("ed4f3665c72229886e4bdae876233892f8a7b85e5cee93da56be71d9056f6654"),
        Node(
          DoubleSha256Digest("2dc060204deff176f6e366319777d4db76546c17c07f23ebeece4bf0e66fd686"),
          Node(
            DoubleSha256Digest("8023ea73c6c1e643d1604585abbfd997308a5759baa4d2d4a2ee876d71a5780f"),
            Leaf(DoubleSha256Digest("e4aeaf729035a7fb939e12c4f6a2072a9b2e7da784207ce7852d398593210a45")),
            Leaf(DoubleSha256Digest("010506d2103d0feb477224926eedaf3d7478fe3d93b54bd24e5eb2c0adc309b3"))),
          Node(
            DoubleSha256Digest("79a03d2d9f1c5c97772974c9ef9297e6e2bce0271ca95d40bb598c6156c3d6e0"),
            Leaf(DoubleSha256Digest("77352045b2995c9e0dfff9089e5563cd13914eb4b0723cdd54675c5c3f1c4f6a")),
            Leaf(DoubleSha256Digest("7ae10c30932c07e4ed25abab233565f9ab279eabbcd60e1bc028c6cdc400361b")))),
        Leaf(DoubleSha256Digest("8ca2e6b66c55fbb63cb7c9b5ccd19be508034eedcd8511d216b9fe93aafc2ceb"))),
        UInt32(6), List(true, true, true, false, true, true, false, true, false, false, false, false, false, false, false, false),
        List(
          DoubleSha256Digest("e4aeaf729035a7fb939e12c4f6a2072a9b2e7da784207ce7852d398593210a45"),
          DoubleSha256Digest("010506d2103d0feb477224926eedaf3d7478fe3d93b54bd24e5eb2c0adc309b3"),
          DoubleSha256Digest("77352045b2995c9e0dfff9089e5563cd13914eb4b0723cdd54675c5c3f1c4f6a"),
          DoubleSha256Digest("7ae10c30932c07e4ed25abab233565f9ab279eabbcd60e1bc028c6cdc400361b"),
          DoubleSha256Digest("8ca2e6b66c55fbb63cb7c9b5ccd19be508034eedcd8511d216b9fe93aafc2ceb")))),
      List(
        DoubleSha256Digest("010506d2103d0feb477224926eedaf3d7478fe3d93b54bd24e5eb2c0adc309b3"),
        DoubleSha256Digest("7ae10c30932c07e4ed25abab233565f9ab279eabbcd60e1bc028c6cdc400361b")))

    val hex = merkleBlock.hex
    val actualMerkleBlock = MerkleBlock(hex)
    actualMerkleBlock.partialMerkleTree.bits must be(merkleBlock.partialMerkleTree.bits)
    actualMerkleBlock must be(merkleBlock)
  }

  it must "serialize and deserialize a merkle block from bitcoinj" in {
    //https://github.com/bitcoinj/bitcoinj/blob/840df06b79beac1b984e6e247e90fcdedc4ad6e0/core/src/test/java/org/bitcoinj/core/FilteredBlockAndPartialMerkleTreeTests.java#L73
    //from this block
    //000000000000dab0130bbcc991d3d7ae6b81aa6f50a798888dfe62337458dc45
    val hex = "0100000079cda856b143d9db2c1caff01d1aecc8630d30625d10e8b4b8b0000000000000b50cc069d6a3e33e3ff84a5c41d9d3febe7c770fdcc96b2c3ff60abe184f196367291b4d4c86041b8fa45d630100000001b50cc069d6a3e33e3ff84a5c41d9d3febe7c770fdcc96b2c3ff60abe184f19630101"
    val merkleBlock = MerkleBlock(hex)

    merkleBlock.hex must be(hex)
    merkleBlock.blockHeader.hash.hex must be(BitcoinSUtil.flipEndianness("000000000000dab0130bbcc991d3d7ae6b81aa6f50a798888dfe62337458dc45"))

    val matches = merkleBlock.partialMerkleTree.extractMatches

    matches must be(Seq(DoubleSha256Digest(BitcoinSUtil.flipEndianness("63194f18be0af63f2c6bc9dc0f777cbefed3d9415c4af83f3ee3a3d669c00cb5"))))
  }

  it must "serialize and deserialize a merkle block with two bytes worth of bit flags" in {
    //https://github.com/bitcoinj/bitcoinj/blob/840df06b79beac1b984e6e247e90fcdedc4ad6e0/core/src/test/java/org/bitcoinj/core/FilteredBlockAndPartialMerkleTreeTests.java#L129
    val hex = "0100000006e533fd1ada86391f3f6c343204b0d278d4aaec1c0b20aa27ba0300000000006abbb3eb3d733a9fe18967fd7d4c117e4ccbbac5bec4d910d900b3ae0793e77f54241b4d4c86041b4089cc9b0c000000084c30b63cfcdc2d35e3329421b9805ef0c6565d35381ca857762ea0b3a5a128bbca5065ff9617cbcba45eb23726df6498a9b9cafed4f54cbab9d227b0035ddefbbb15ac1d57d0182aaee61c74743a9c4f785895e563909bafec45c9a2b0ff3181d77706be8b1dcc91112eada86d424e2d0a8907c3488b6e44fda5a74a25cbc7d6bb4fa04245f4ac8a1a571d5537eac24adca1454d65eda446055479af6c6d4dd3c9ab658448c10b6921b7a4ce3021eb22ed6bb6a7fde1e5bcc4b1db6615c6abc5ca042127bfaf9f44ebce29cb29c6df9d05b47f35b2edff4f0064b578ab741fa78276222651209fe1a2c4c0fa1c58510aec8b090dd1eb1f82f9d261b8273b525b02ff1a"
    val merkleBlock = MerkleBlock(hex)
    merkleBlock.hex must be(hex)
  }
}

