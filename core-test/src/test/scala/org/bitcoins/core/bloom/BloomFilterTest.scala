package org.bitcoins.core.bloom

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, CryptoUtil}
import org.scalatest.{FlatSpec, MustMatchers}
import scodec.bits.ByteVector

import scala.util.Try

/**
  * Created by chris on 8/3/16.
  */
class BloomFilterTest extends FlatSpec with MustMatchers {
  private val logger = BitcoinSLogger.logger
  "BloomFilter" must "create a bloom filter, insert a few elements, then serialize and deserialize it" in {
    //test case in bitcoin core
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/bloom_tests.cpp#L28
    val filter = BloomFilter(3, 0.01, UInt32.zero, BloomUpdateAll)
    //hex is from bitcoin core
    filter.hex must be("03000000050000000000000001")
    val hash = Sha256Hash160Digest("99108ad8ed9bb6274d3980bab5a85c048f0950c8")
    val newFilter = filter.insert(hash)
    //hex from bitcoin core
    newFilter.hex must be("03010098050000000000000001")
    newFilter.contains(hash) must be(true)

    val hash1BitDifferent =
      Sha256Hash160Digest("19108ad8ed9bb6274d3980bab5a85c048f0950c8")
    newFilter.contains(hash1BitDifferent) must be(false)

    val hash1 = Sha256Hash160Digest("b5a2c786d9ef4658287ced5914b37a1b4aa32eee")
    val filter2 = newFilter.insert(hash1)

    filter2.contains(hash1) must be(true)

    val hash2 = Sha256Hash160Digest("b9300670b4c5366e95b2699e8b18bc75e5f729c5")
    val filter3 = filter2.insert(hash2)
    filter3.contains(hash2) must be(true)

    filter3.hex must be("03614e9b050000000000000001")

    val filter4 = BloomFilter.fromBytes(filter3.bytes)
    (filter4 == filter3) must be(true)
  }

  it must "create a bloom filter with a tweak then insert elements and serialize it" in {
    //mimics this test case from core https://github.com/bitcoin/bitcoin/blob/master/src/test/bloom_tests.cpp#L59
    val filter = BloomFilter(3, 0.01, UInt32(2147483649L), BloomUpdateAll)

    val hash1 = Sha256Hash160Digest("99108ad8ed9bb6274d3980bab5a85c048f0950c8")
    val filter1 = filter.insert(hash1)
    filter1.contains(hash1) must be(true)
    //one bit different
    filter1.contains(
      Sha256Hash160Digest("19108ad8ed9bb6274d3980bab5a85c048f0950c8")) must be(
      false)

    val hash2 = Sha256Hash160Digest("b5a2c786d9ef4658287ced5914b37a1b4aa32eee")
    val filter2 = filter1.insert(hash2)
    filter2.contains(hash2) must be(true)

    val hash3 = Sha256Hash160Digest("b9300670b4c5366e95b2699e8b18bc75e5f729c5")
    val filter3 = filter2.insert(hash3)
    filter3.contains(hash3) must be(true)

    filter3.hex must be("03ce4299050000000100008001")
  }

  it must "insert a key & it's address into our bloom filter and the check to make sure it contains them" in {
    val filter = BloomFilter(2, 0.001, UInt32.zero, BloomUpdateAll)
    val privKey = ECPrivateKey.fromWIFToPrivateKey(
      "5Kg1gnAjaLfKiwhhPpGS3QfRg2m6awQvaj98JCZBZQ5SuS2F15C")
    logger.debug("PrivKey: " + privKey.hex)
    require(
      privKey.hex == "f49addfd726a59abde172c86452f5f73038a02f4415878dc14934175e8418aff")
    val pubKey = privKey.publicKey
    logger.debug("PubKey being inserted into filter: " + pubKey.hex)
    val filter1 = filter.insert(pubKey.bytes)
    //hex is from bitcoin core
    filter1.hex must be("0302c12b080000000000000001")
    val keyId = CryptoUtil.sha256Hash160(pubKey.bytes)
    val filter2 = filter1.insert(keyId.bytes)

    filter2.hex must be("038fc16b080000000000000001")
  }

  it must "test the isRelevant part of isRelevantAndUpdate inside of core" in {
    //mimics this test case in core
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/bloom_tests.cpp#L114
    val creditingTx = Transaction(
      "01000000010b26e9b7735eb6aabdf358bab62f9816a21ba9ebdb719d5299e88607d722c190000000008b4830450220070aca44506c5cef3a16ed519d7c3c39f8aab192c4e1c90d065f37b8a4af6141022100a8e160b856c2d43d27d8fba71e5aef6405b8643ac4cb7cb3c462aced7f14711a0141046d11fee51b0e60666d5049a9101a72741df480b96ee26488a4d3466b95c9a40ac5eeef87e10a5cd336c19a84565f80fa6c547957b7700ff4dfbdefe76036c339ffffffff021bff3d11000000001976a91404943fdd508053c75000106d3bc6e2754dbcff1988ac2f15de00000000001976a914a266436d2965547608b9e15d9032a7b9d64fa43188ac00000000")

    val spendingTx = Transaction(
      "01000000016bff7fcd4f8565ef406dd5d63d4ff94f318fe82027fd4dc451b04474019f74b4000000008c493046022100da0dc6aecefe1e06efdf05773757deb168820930e3b0d03f46f5fcf150bf990c022100d25b5c87040076e4f253f8262e763e2dd51e7ff0be157727c4bc42807f17bd39014104e6c26ef67dc610d2cd192484789a6cf9aea9930b944b7e2db5342b9d9e5b9ff79aff9a2ee1978dd7fd01dfc522ee02283d3b06a9d03acf8096968d7dbb0f9178ffffffff028ba7940e000000001976a914badeecfdef0507247fc8f74241d73bc039972d7b88ac4094a802000000001976a914c10932483fec93ed51f5fe95e72559f2cc7043f988ac00000000")

    val filter = BloomFilter(10, 0.000001, UInt32.zero, BloomUpdateAll)

    val filter1 = filter.insert(creditingTx.txId)

    filter1.isRelevant(creditingTx) must be(true)

    val filter2 = BloomFilter(10, 0.000001, UInt32.zero, BloomUpdateAll)

    //byte reversed tx hash
    val filter3 = filter2.insert(
      DoubleSha256Digest(
        "6bff7fcd4f8565ef406dd5d63d4ff94f318fe82027fd4dc451b04474019f74b4"))
    filter3.isRelevant(creditingTx) must be(true)

    val filter4 = BloomFilter(10, 0.000001, UInt32.zero, BloomUpdateAll)
    //insert a digital signature in our bloom filter for the spendingTx
    val filter5 = filter4.insert(BitcoinSUtil.decodeHex(
      "30450220070aca44506c5cef3a16ed519d7c3c39f8aab192c4e1c90d065f37b8a4af6141022100a8e160b856c2d43d27d8fba71e5aef6405b8643ac4cb7cb3c462aced7f14711a01"))
    filter5.isRelevant(creditingTx) must be(true)

    val filter6 = BloomFilter(10, 0.000001, UInt32.zero, BloomUpdateAll)
    //insert the pubkey of spendingTx in the bloom filter
    val pubKey = ECPublicKey(
      "046d11fee51b0e60666d5049a9101a72741df480b96ee26488a4d3466b95c9a40ac5eeef87e10a5cd336c19a84565f80fa6c547957b7700ff4dfbdefe76036c339")
    val filter7 = filter6.insert(pubKey.bytes)
    filter7.isRelevant(creditingTx) must be(true)

    val filter8 = BloomFilter(10, 0.000001, UInt32.zero, BloomUpdateAll)
    val filter9 = filter8.insert(
      Sha256Hash160Digest("04943fdd508053c75000106d3bc6e2754dbcff19"))
    filter9.isRelevant(creditingTx) must be(true)
    //update the bloom filter to add the outputs inside of the crediting tx
    //this is what the core test case really does, but since we separated the isRelevant and update parts, we need
    //to call update explicitly
    val filter10 = filter9.update(creditingTx)
    filter10.isRelevant(spendingTx) must be(true)

    val filter11 = BloomFilter(10, 0.000001, UInt32.zero, BloomUpdateAll)
    val filter12 = filter11.insert(
      Sha256Hash160Digest("a266436d2965547608b9e15d9032a7b9d64fa431"))
    filter12.isRelevant(creditingTx) must be(true)

    val filter13 = BloomFilter(10, 0.000001, UInt32.zero, BloomUpdateAll)
    val hash = Sha256Hash160Digest("a266436d2965547608b9e15d9032a7b9d64fa431")
    val filter14 = filter13.insert(hash)
    filter14.contains(hash) must be(true)

    val filter15 = BloomFilter(10, 0.000001, UInt32.zero, BloomUpdateAll)
    val outPoint = TransactionOutPoint(
      DoubleSha256Digest(
        BitcoinSUtil.flipEndianness(
          "90c122d70786e899529d71dbeba91ba216982fb6ba58f3bdaab65e73b7e9260b")),
      UInt32.zero)
    val filter16 = filter15.insert(outPoint)
    filter16.hex must be(
      "230008000000000100000000200040304001000020000000100800050801000400800024130000000000000001")
    filter16.isRelevant(creditingTx) must be(true)

    val filter17 = BloomFilter(10, 0.000001, UInt32.zero, BloomUpdateAll)
    //random tx hash
    val filter18 = filter17.insert(
      DoubleSha256Digest(
        "00000009e784f32f62ef849763d4f45b98e07ba658647343b915ff832b110436"))
    filter18.isRelevant(creditingTx) must be(false)

    val filter19 = BloomFilter(10, 0.000001, UInt32.zero, BloomUpdateAll)
    //makes sure filter does not match a random outpoint
    val randomOutPoint = TransactionOutPoint(
      DoubleSha256Digest(
        "90c122d70786e899529d71dbeba91ba216982fb6ba58f3bdaab65e73b7e9260b"),
      UInt32.one)
    val filter20 = filter19.insert(randomOutPoint)
    filter20.isRelevant(creditingTx) must be(false)

    val filter21 = BloomFilter(10, 0.000001, UInt32.zero, BloomUpdateAll)
    val secondRandomOutPoint = TransactionOutPoint(
      DoubleSha256Digest(
        BitcoinSUtil.flipEndianness(
          "000000d70786e899529d71dbeba91ba216982fb6ba58f3bdaab65e73b7e9260b")),
      UInt32.zero)
    val filter22 = filter21.insert(secondRandomOutPoint)
    filter22.hex must be(
      "230090f00000004000000005040000000004000400000000100101000000008002040000130000000000000001")
    filter22.isRelevant(creditingTx) must be(false)
  }

  it must "find a transaction is relevant if we have inserted a public key into our bloom filter" in {
    //mimics this part of a merkle block test case in core
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/bloom_tests.cpp#L314-L330
    val block = Block(
      "0100000075616236cc2126035fadb38deb65b9102cc2c41c09cdf29fc051906800000000fe7d5e12ef0ff901f6050211249919b1c0653771832b3a80c66cea42847f0ae1d4d26e49ffff001d00f0a4410401000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0804ffff001d029105ffffffff0100f2052a010000004341046d8709a041d34357697dfcb30a9d05900a6294078012bf3bb09c6f9b525f1d16d5503d7905db1ada9501446ea00728668fc5719aa80be2fdfc8a858a4dbdd4fbac00000000010000000255605dc6f5c3dc148b6da58442b0b2cd422be385eab2ebea4119ee9c268d28350000000049483045022100aa46504baa86df8a33b1192b1b9367b4d729dc41e389f2c04f3e5c7f0559aae702205e82253a54bf5c4f65b7428551554b2045167d6d206dfe6a2e198127d3f7df1501ffffffff55605dc6f5c3dc148b6da58442b0b2cd422be385eab2ebea4119ee9c268d2835010000004847304402202329484c35fa9d6bb32a55a70c0982f606ce0e3634b69006138683bcd12cbb6602200c28feb1e2555c3210f1dddb299738b4ff8bbe9667b68cb8764b5ac17b7adf0001ffffffff0200e1f505000000004341046a0765b5865641ce08dd39690aade26dfbf5511430ca428a3089261361cef170e3929a68aee3d8d4848b0c5111b0a37b82b86ad559fd2a745b44d8e8d9dfdc0cac00180d8f000000004341044a656f065871a353f216ca26cef8dde2f03e8c16202d2e8ad769f02032cb86a5eb5e56842e92e19141d60a01928f8dd2c875a390f67c1f6c94cfc617c0ea45afac0000000001000000025f9a06d3acdceb56be1bfeaa3e8a25e62d182fa24fefe899d1c17f1dad4c2028000000004847304402205d6058484157235b06028c30736c15613a28bdb768ee628094ca8b0030d4d6eb0220328789c9a2ec27ddaec0ad5ef58efded42e6ea17c2e1ce838f3d6913f5e95db601ffffffff5f9a06d3acdceb56be1bfeaa3e8a25e62d182fa24fefe899d1c17f1dad4c2028010000004a493046022100c45af050d3cea806cedd0ab22520c53ebe63b987b8954146cdca42487b84bdd6022100b9b027716a6b59e640da50a864d6dd8a0ef24c76ce62391fa3eabaf4d2886d2d01ffffffff0200e1f505000000004341046a0765b5865641ce08dd39690aade26dfbf5511430ca428a3089261361cef170e3929a68aee3d8d4848b0c5111b0a37b82b86ad559fd2a745b44d8e8d9dfdc0cac00180d8f000000004341046a0765b5865641ce08dd39690aade26dfbf5511430ca428a3089261361cef170e3929a68aee3d8d4848b0c5111b0a37b82b86ad559fd2a745b44d8e8d9dfdc0cac000000000100000002e2274e5fea1bf29d963914bd301aa63b64daaf8a3e88f119b5046ca5738a0f6b0000000048473044022016e7a727a061ea2254a6c358376aaa617ac537eb836c77d646ebda4c748aac8b0220192ce28bf9f2c06a6467e6531e27648d2b3e2e2bae85159c9242939840295ba501ffffffffe2274e5fea1bf29d963914bd301aa63b64daaf8a3e88f119b5046ca5738a0f6b010000004a493046022100b7a1a755588d4190118936e15cd217d133b0e4a53c3c15924010d5648d8925c9022100aaef031874db2114f2d869ac2de4ae53908fbfea5b2b1862e181626bb9005c9f01ffffffff0200e1f505000000004341044a656f065871a353f216ca26cef8dde2f03e8c16202d2e8ad769f02032cb86a5eb5e56842e92e19141d60a01928f8dd2c875a390f67c1f6c94cfc617c0ea45afac00180d8f000000004341046a0765b5865641ce08dd39690aade26dfbf5511430ca428a3089261361cef170e3929a68aee3d8d4848b0c5111b0a37b82b86ad559fd2a745b44d8e8d9dfdc0cac00000000")
    val txs = block.transactions
    val pubKey = ECPublicKey(
      "044a656f065871a353f216ca26cef8dde2f03e8c16202d2e8ad769f02032cb86a5eb5e56842e92e19141d60a01928f8dd2c875a390f67c1f6c94cfc617c0ea45af")
    val filter =
      BloomFilter(1, 0.00001, UInt32.zero, BloomUpdateNone).insert(pubKey.bytes)

    // Match an output from the second transaction (the pubkey for address 1DZTzaBHUDM7T3QvUKBz4qXMRpkg8jsfB5)
    // This should not match the third transaction though it spends the output matched
    // It will match the fourth transaction, which has another pay-to-pubkey output to the same address
    filter.isRelevant(txs.head) must be(false)
    filter.isRelevant(txs(1)) must be(true)
    filter.isRelevant(txs(2)) must be(false)
    filter.isRelevant(txs(3)) must be(true)
  }

  it must "update a bloom filter correctly when the BloomUpdateP2PKOnly flag is set" in {
    //follows this test case inside of core
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/bloom_tests.cpp#L423
    val block = Block(
      "0100000082bb869cf3a793432a66e826e05a6fc37469f8efb7421dc880670100000000007f16c5962e8bd963659c793ce370d95f093bc7e367117b3c30c1f8fdd0d9728776381b4d4c86041b554b85290701000000010000000000000000000000000000000000000000000000000000000000000000ffffffff07044c86041b0136ffffffff0100f2052a01000000434104eaafc2314def4ca98ac970241bcab022b9c1e1f4ea423a20f134c876f2c01ec0f0dd5b2e86e7168cefe0d81113c3807420ce13ad1357231a2252247d97a46a91ac000000000100000001bcad20a6a29827d1424f08989255120bf7f3e9e3cdaaa6bb31b0737fe048724300000000494830450220356e834b046cadc0f8ebb5a8a017b02de59c86305403dad52cd77b55af062ea10221009253cd6c119d4729b77c978e1e2aa19f5ea6e0e52b3f16e32fa608cd5bab753901ffffffff02008d380c010000001976a9142b4b8072ecbba129b6453c63e129e643207249ca88ac0065cd1d000000001976a9141b8dd13b994bcfc787b32aeadf58ccb3615cbd5488ac000000000100000003fdacf9b3eb077412e7a968d2e4f11b9a9dee312d666187ed77ee7d26af16cb0b000000008c493046022100ea1608e70911ca0de5af51ba57ad23b9a51db8d28f82c53563c56a05c20f5a87022100a8bdc8b4a8acc8634c6b420410150775eb7f2474f5615f7fccd65af30f310fbf01410465fdf49e29b06b9a1582287b6279014f834edc317695d125ef623c1cc3aaece245bd69fcad7508666e9c74a49dc9056d5fc14338ef38118dc4afae5fe2c585caffffffff309e1913634ecb50f3c4f83e96e70b2df071b497b8973a3e75429df397b5af83000000004948304502202bdb79c596a9ffc24e96f4386199aba386e9bc7b6071516e2b51dda942b3a1ed022100c53a857e76b724fc14d45311eac5019650d415c3abb5428f3aae16d8e69bec2301ffffffff2089e33491695080c9edc18a428f7d834db5b6d372df13ce2b1b0e0cbcb1e6c10000000049483045022100d4ce67c5896ee251c810ac1ff9ceccd328b497c8f553ab6e08431e7d40bad6b5022033119c0c2b7d792d31f1187779c7bd95aefd93d90a715586d73801d9b47471c601ffffffff0100714460030000001976a914c7b55141d097ea5df7a0ed330cf794376e53ec8d88ac0000000001000000045bf0e214aa4069a3e792ecee1e1bf0c1d397cde8dd08138f4b72a00681743447000000008b48304502200c45de8c4f3e2c1821f2fc878cba97b1e6f8807d94930713aa1c86a67b9bf1e40221008581abfef2e30f957815fc89978423746b2086375ca8ecf359c85c2a5b7c88ad01410462bb73f76ca0994fcb8b4271e6fb7561f5c0f9ca0cf6485261c4a0dc894f4ab844c6cdfb97cd0b60ffb5018ffd6238f4d87270efb1d3ae37079b794a92d7ec95ffffffffd669f7d7958d40fc59d2253d88e0f248e29b599c80bbcec344a83dda5f9aa72c000000008a473044022078124c8beeaa825f9e0b30bff96e564dd859432f2d0cb3b72d3d5d93d38d7e930220691d233b6c0f995be5acb03d70a7f7a65b6bc9bdd426260f38a1346669507a3601410462bb73f76ca0994fcb8b4271e6fb7561f5c0f9ca0cf6485261c4a0dc894f4ab844c6cdfb97cd0b60ffb5018ffd6238f4d87270efb1d3ae37079b794a92d7ec95fffffffff878af0d93f5229a68166cf051fd372bb7a537232946e0a46f53636b4dafdaa4000000008c493046022100c717d1714551663f69c3c5759bdbb3a0fcd3fab023abc0e522fe6440de35d8290221008d9cbe25bffc44af2b18e81c58eb37293fd7fe1c2e7b46fc37ee8c96c50ab1e201410462bb73f76ca0994fcb8b4271e6fb7561f5c0f9ca0cf6485261c4a0dc894f4ab844c6cdfb97cd0b60ffb5018ffd6238f4d87270efb1d3ae37079b794a92d7ec95ffffffff27f2b668859cd7f2f894aa0fd2d9e60963bcd07c88973f425f999b8cbfd7a1e2000000008c493046022100e00847147cbf517bcc2f502f3ddc6d284358d102ed20d47a8aa788a62f0db780022100d17b2d6fa84dcaf1c95d88d7e7c30385aecf415588d749afd3ec81f6022cecd701410462bb73f76ca0994fcb8b4271e6fb7561f5c0f9ca0cf6485261c4a0dc894f4ab844c6cdfb97cd0b60ffb5018ffd6238f4d87270efb1d3ae37079b794a92d7ec95ffffffff0100c817a8040000001976a914b6efd80d99179f4f4ff6f4dd0a007d018c385d2188ac000000000100000001834537b2f1ce8ef9373a258e10545ce5a50b758df616cd4356e0032554ebd3c4000000008b483045022100e68f422dd7c34fdce11eeb4509ddae38201773dd62f284e8aa9d96f85099d0b002202243bd399ff96b649a0fad05fa759d6a882f0af8c90cf7632c2840c29070aec20141045e58067e815c2f464c6a2a15f987758374203895710c2d452442e28496ff38ba8f5fd901dc20e29e88477167fe4fc299bf818fd0d9e1632d467b2a3d9503b1aaffffffff0280d7e636030000001976a914f34c3e10eb387efe872acb614c89e78bfca7815d88ac404b4c00000000001976a914a84e272933aaf87e1715d7786c51dfaeb5b65a6f88ac00000000010000000143ac81c8e6f6ef307dfe17f3d906d999e23e0189fda838c5510d850927e03ae7000000008c4930460221009c87c344760a64cb8ae6685a3eec2c1ac1bed5b88c87de51acd0e124f266c16602210082d07c037359c3a257b5c63ebd90f5a5edf97b2ac1c434b08ca998839f346dd40141040ba7e521fa7946d12edbb1d1e95a15c34bd4398195e86433c92b431cd315f455fe30032ede69cad9d1e1ed6c3c4ec0dbfced53438c625462afb792dcb098544bffffffff0240420f00000000001976a9144676d1b820d63ec272f1900d59d43bc6463d96f888ac40420f00000000001976a914648d04341d00d7968b3405c034adc38d4d8fb9bd88ac00000000010000000248cc917501ea5c55f4a8d2009c0567c40cfe037c2e71af017d0a452ff705e3f1000000008b483045022100bf5fdc86dc5f08a5d5c8e43a8c9d5b1ed8c65562e280007b52b133021acd9acc02205e325d613e555f772802bf413d36ba807892ed1a690a77811d3033b3de226e0a01410429fa713b124484cb2bd7b5557b2c0b9df7b2b1fee61825eadc5ae6c37a9920d38bfccdc7dc3cb0c47d7b173dbc9db8d37db0a33ae487982c59c6f8606e9d1791ffffffff41ed70551dd7e841883ab8f0b16bf04176b7d1480e4f0af9f3d4c3595768d068000000008b4830450221008513ad65187b903aed1102d1d0c47688127658c51106753fed0151ce9c16b80902201432b9ebcb87bd04ceb2de66035fbbaf4bf8b00d1cfe41f1a1f7338f9ad79d210141049d4cf80125bf50be1709f718c07ad15d0fc612b7da1f5570dddc35f2a352f0f27c978b06820edca9ef982c35fda2d255afba340068c5035552368bc7200c1488ffffffff0100093d00000000001976a9148edb68822f1ad580b043c7b3df2e400f8699eb4888ac00000000")
    // the output for the first transaction in this block is a pay-to-pubkey tx to this pubkey
    val pubKey = BitcoinSUtil.decodeHex(
      "04eaafc2314def4ca98ac970241bcab022b9c1e1f4ea423a20f134c876f2c01ec0f0dd5b2e86e7168cefe0d81113c3807420ce13ad1357231a2252247d97a46a91")
    // ...and the output address of the 4th transaction
    val output =
      BitcoinSUtil.decodeHex("b6efd80d99179f4f4ff6f4dd0a007d018c385d21")
    val filter = BloomFilter(10, 0.000001, UInt32.zero, BloomUpdateP2PKOnly)
      .insert(pubKey)
      .insert(output)

    filter.isRelevant(block.transactions.head) must be(true)
    //this transaction should be updated in the fitler because the BloomUpdateP2PKOnly flag is set
    val txUpdatedFilter = filter.update(block.transactions.head)
    //outpoint of the tx whose scriptPubKey we should match after updating the filter
    val outPoint =
      TransactionOutPoint(block.transactions.head.txId, UInt32.zero)
    txUpdatedFilter.contains(outPoint) must be(true)

    //update the filter with a tx whose output is NOT pay-to-pubkey
    val filterNonP2PKTx = txUpdatedFilter.update(block.transactions(4))

    val outPoint2 = TransactionOutPoint(block.transactions(4).txId, UInt32.zero)
    filterNonP2PKTx.contains(outPoint2) must be(false)
  }

  it must "successfully create or fail to create a bloom flag" in {
    (BloomFlag(0.toByte) == BloomUpdateNone) must be(true)
    (BloomFlag(1.toByte) == BloomUpdateAll) must be(true)
    (BloomFlag(2.toByte) == BloomUpdateP2PKOnly) must be(true)
    (BloomFlag.fromBytes(ByteVector(BloomUpdateNone.byte)) == BloomUpdateNone) must be(
      true)
    Try(BloomFlag(Int.MaxValue.toByte)).isFailure must be(true)
  }
}
