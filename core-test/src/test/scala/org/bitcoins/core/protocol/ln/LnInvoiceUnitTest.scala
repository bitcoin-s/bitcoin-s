package org.bitcoins.core.protocol.ln

import org.bitcoins.core.number.{UInt32, UInt64, UInt8}
import org.bitcoins.core.protocol.ln.LnParams.{
  LnBitcoinMainNet,
  LnBitcoinTestNet
}
import org.bitcoins.core.protocol.ln.channel.ShortChannelId
import org.bitcoins.core.protocol.ln.currency.{
  MicroBitcoins,
  MilliBitcoins,
  MilliSatoshis
}
import org.bitcoins.core.protocol.ln.fee.{
  FeeBaseMSat,
  FeeProportionalMillionths
}
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.ln.routing.LnRoute
import org.bitcoins.core.protocol.{Bech32Address, P2PKHAddress, P2SHAddress}
import org.bitcoins.core.util.Bech32
import org.bitcoins.crypto._
import org.bitcoins.testkitcore.gen.ln.LnInvoiceGen
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.ByteVector

class LnInvoiceUnitTest extends BitcoinSUnitTest {
  behavior of "LnInvoice"

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  val hrpEmpty = LnHumanReadablePart(LnBitcoinMainNet)

  val hrpMicro =
    LnHumanReadablePart(LnBitcoinMainNet, Some(MicroBitcoins(2500)))
  val hrpMilli = LnHumanReadablePart(LnBitcoinMainNet, Some(MilliBitcoins(20)))

  val hrpTestNetMilli =
    LnHumanReadablePart(LnBitcoinTestNet, Some(MilliBitcoins(20)))
  val time = UInt64(1496314658)

  val paymentHash = Sha256Digest.fromHex(
    "0001020304050607080900010203040506070809000102030405060708090102")
  val paymentTag = LnTag.PaymentHashTag(paymentHash)

  val description = {
    ("One piece of chocolate cake, one icecream cone, one pickle, one slice of swiss cheese, " +
      "one slice of salami, one lollypop, one piece of cherry pie, one sausage, one cupcake, " +
      "and one slice of watermelon").getBytes()
  }
  val descriptionHash = CryptoUtil.sha256(ByteVector(description))

  val descpriptionHashTag = Right(LnTag.DescriptionHashTag(descriptionHash))

  it must "parse BOLT11 example 1" in {
    //BOLT11 Example #1

    val descriptionTagE =
      Left(LnTag.DescriptionTag("Please consider supporting this project"))
    val lnTags = LnTaggedFields(paymentHash = paymentTag,
                                descriptionOrHash = descriptionTagE)

    val sigData =
      "6c6e62630b25fe64410d00004080c1014181c20240004080c1014181c20240004080c1014181c202404081a1fa83632b0b9b29031b7b739b4b232b91039bab83837b93a34b733903a3434b990383937b532b1ba0"
    /*    val hashSigData = Sha256Digest.fromHex(
      "c3d4e83f646fa79a393d75277b1d858db1d1f7ab7137dcb7835db2ecd518e1c9")*/

    val signature = ECDigitalSignature.fromRS(
      "38ec6891345e204145be8a3a99de38e98a39d6a569434e1845c8af7205afcfcc7f425fcd1463e93c32881ead0d6e356d467ec8c02553f9aab15e5738b11f127f")
    val version = UInt8.zero
    val lnSig = LnInvoiceSignature(version, signature)

    val invoice = LnInvoice(hrpEmpty, time, lnTags, lnSig)

    invoice.signatureData.toHex must be(sigData)

    val serialized = invoice.toString
    serialized must be(
      "lnbc1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpl2pkx2ctnv5sxxmmwwd5kgetjypeh2ursdae8g6twvus8g6rfwvs8qun0dfjkxaq8rkx3yf5tcsyz3d73gafnh3cax9rn449d9p5uxz9ezhhypd0elx87sjle52x86fux2ypatgddc6k63n7erqz25le42c4u4ecky03ylcqca784w")

    val deserialized = LnInvoice.fromStringT(serialized)

    deserialized.get.toString must be(invoice.toString)
  }

  it must "parse BOLT11 example 2" in {
    //BOLT11 Example #2

    val descriptionTagE = Left(LnTag.DescriptionTag("1 cup coffee"))
    val expiryTimeTag = LnTag.ExpiryTimeTag(UInt32(60))
    val lnTags = LnTaggedFields(paymentTag,
                                descriptionOrHash = descriptionTagE,
                                expiryTime = Some(expiryTimeTag))

    val signature = ECDigitalSignature.fromRS(
      "e89639ba6814e36689d4b91bf125f10351b55da057b00647a8dabaeb8a90c95f160f9d5a6e0f79d1fc2b964238b944e2fa4aa677c6f020d466472ab842bd750e")
    val version = UInt8.one
    val lnSig = LnInvoiceSignature(version, signature)

    val invoice = LnInvoice(hrpMicro, time, lnTags, lnSig)

    val serialized = invoice.toString

    serialized must be(
      "lnbc2500u1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5xysxxatsyp3k7enxv4jsxqzpuaztrnwngzn3kdzw5hydlzf03qdgm2hdq27cqv3agm2awhz5se903vruatfhq77w3ls4evs3ch9zw97j25emudupq63nyw24cg27h2rspfj9srp")

    val deserialized = LnInvoice.fromStringT(serialized)

    deserialized.get.toString must be(invoice.toString)
  }

  it must "parse BOLT11 example 3" in {
    //BOLT11 Example #3 - Description field does not encode correctly due to Japanese letters

    val descriptionTagE = Left(LnTag.DescriptionTag("ナンセンス 1杯"))
    val expiryTag = LnTag.ExpiryTimeTag(UInt32(60))
    val lnTags = LnTaggedFields(
      paymentHash = paymentTag,
      secret = None,
      descriptionOrHash = descriptionTagE,
      nodeId = None,
      expiryTime = Some(expiryTag),
      cltvExpiry = None,
      fallbackAddress = None,
      routingInfo = None,
      features = None
    )

    val signature = ECDigitalSignature.fromRS(
      "259f04511e7ef2aa77f6ff04d51b4ae9209504843e5ab9672ce32a153681f687515b73ce57ee309db588a10eb8e41b5a2d2bc17144ddf398033faa49ffe95ae6")
    val version = UInt8.zero
    val lnSig = LnInvoiceSignature(version, signature)

    val invoice = LnInvoice(hrpMicro, time, lnTags, lnSig)

    val serialized = invoice.toString

    invoice.toString must be(
      "lnbc2500u1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpquwpc4curk03c9wlrswe78q4eyqc7d8d0xqzpuyk0sg5g70me25alkluzd2x62aysf2pyy8edtjeevuv4p2d5p76r4zkmneet7uvyakky2zr4cusd45tftc9c5fh0nnqpnl2jfll544esqchsrny")

    val deserialized = LnInvoice.fromStringT(serialized)

    deserialized.get must be(invoice)
  }

  it must "parse BOLT11 example 4" in {
    //BOLT11 Example #4

    val descriptionHash = Sha256Digest.fromHex(
      "3925b6f67e2c340036ed12093dd44e0368df1b6ea26c53dbe4811f58fd5db8c1")
    val descriptionHashTagE = Right(LnTag.DescriptionHashTag(descriptionHash))
    val lnTags = LnTaggedFields(paymentHash = paymentTag,
                                descriptionOrHash = descriptionHashTagE,
                                None,
                                None,
                                None,
                                None,
                                None)

    val signature = ECDigitalSignature.fromRS(
      "c63486e81f8c878a105bc9d959af1973854c4dc552c4f0e0e0c7389603d6bdc67707bf6be992a8ce7bf50016bb41d8a9b5358652c4960445a170d049ced4558c")
    val version = UInt8.zero
    val lnSig = LnInvoiceSignature(version, signature)

    val invoice = LnInvoice(hrpMilli, time, lnTags, lnSig)

    val serialized = invoice.toString

    invoice.toString must be(
      "lnbc20m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqscc6gd6ql3jrc5yzme8v4ntcewwz5cnw92tz0pc8qcuufvq7khhr8wpald05e92xw006sq94mg8v2ndf4sefvf9sygkshp5zfem29trqq2yxxz7")

    val deserialized = LnInvoice.fromStringT(serialized)

    deserialized.get must be(invoice)
  }

  it must "parse BOLT11 example 5" in {
    //BOLT11 Example #5

    val descriptionHash = Sha256Digest.fromHex(
      "3925b6f67e2c340036ed12093dd44e0368df1b6ea26c53dbe4811f58fd5db8c1")
    val descriptionHashTagE = Right(LnTag.DescriptionHashTag(descriptionHash))
    val fallbackAddr = LnTag.FallbackAddressTag(
      P2PKHAddress.fromString("mk2QpYatsKicvFVuTAQLBryyccRXMUaGHP"))

    val lnTags = LnTaggedFields(paymentHash = paymentTag,
                                descriptionOrHash = descriptionHashTagE,
                                fallbackAddress = Some(fallbackAddr))

    val signature = ECDigitalSignature.fromRS(
      "b6c42b8a61e0dc5823ea63e76ff148ab5f6c86f45f9722af0069c7934daff70d5e315893300774c897995e3a7476c8193693d144a36e2645a0851e6ebafc9d0a")
    val version = UInt8.one
    val lnSig = LnInvoiceSignature(version, signature)

    val invoice = LnInvoice(hrpTestNetMilli, time, lnTags, lnSig)

    val serialized = invoice.toString

    serialized must be(
      "lntb20m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqsfpp3x9et2e20v6pu37c5d9vax37wxq72un98kmzzhznpurw9sgl2v0nklu2g4d0keph5t7tj9tcqd8rexnd07ux4uv2cjvcqwaxgj7v4uwn5wmypjd5n69z2xm3xgksg28nwht7f6zsp2mh7qm")
    //In example #5, the order in which tags are encoded in the invoice has been changed to demonstrate the ability to move tags as needed.
    //For that reason, the example #5 output we are matching against has been modified to fit the order in which we encode our invoices.
    //TODO: Add checksum data to check

    val deserialized = LnInvoice.fromStringT(serialized)

    deserialized.get.toString must be(serialized)
  }
  it must "parse BOLT11 example 6" in {
    val expected =
      "lnbc20m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqsfpp3qjmp7lwpagxun9pygexvgpjdc4jdj85fr9yq20q82gphp2nflc7jtzrcazrra7wwgzxqc8u7754cdlpfrmccae92qgzqvzq2ps8pqqqqqqpqqqqq9qqqvpeuqafqxu92d8lr6fvg0r5gv0heeeqgcrqlnm6jhphu9y00rrhy4grqszsvpcgpy9qqqqqqgqqqqq7qqzqj9n4evl6mr5aj9f58zp6fyjzup6ywn3x6sk8akg5v4tgn2q8g4fhx05wf6juaxu9760yp46454gpg5mtzgerlzezqcqvjnhjh8z3g2qqdhhwkj"

    val fallbackAddr = LnTag.FallbackAddressTag(
      P2PKHAddress.fromString("1RustyRX2oai4EYYDpQGWvEL62BBGqN9T"))

    val signature = ECDigitalSignature.fromRS(
      "91675cb3fad8e9d915343883a49242e074474e26d42c7ed914655689a8074553733e8e4ea5ce9b85f69e40d755a55014536b12323f8b220600c94ef2b9c51428")
    val lnInvoiceSig =
      LnInvoiceSignature(recoverId = UInt8.zero, signature = signature)

    val route1 = LnRoute(
      pubkey = ECPublicKey.fromHex(
        "029e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255"),
      shortChannelID = ShortChannelId.fromHex("0102030405060708"),
      feeBaseMsat = FeeBaseMSat(MilliSatoshis.one),
      feePropMilli = FeeProportionalMillionths(UInt32(20)),
      cltvExpiryDelta = 3
    )

    val route2 = LnRoute(
      pubkey = ECPublicKey.fromHex(
        "039e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255"),
      shortChannelID = ShortChannelId.fromHex("030405060708090a"),
      feeBaseMsat = FeeBaseMSat(MilliSatoshis(2)),
      feePropMilli = FeeProportionalMillionths(UInt32(30)),
      cltvExpiryDelta = 4
    )

    val route = LnTag.RoutingInfo(Vector(route1, route2))

    val lnTags = LnTaggedFields(paymentHash = paymentTag,
                                descriptionOrHash = descpriptionHashTag,
                                fallbackAddress = Some(fallbackAddr),
                                routingInfo = Some(route))

    val lnInvoice = LnInvoice(hrp = hrpMilli,
                              timestamp = time,
                              lnTags = lnTags,
                              signature = lnInvoiceSig)

    val serialized = lnInvoice.toString
    serialized must be(expected)

    val deserialized = LnInvoice.fromStringT(serialized)

    deserialized.get.toString must be(serialized)
  }

  it must "parse BOLT11 example 7 (p2sh fallback addr)" in {
    val expected =
      "lnbc20m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypq" +
        "hp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqsfppj3a24vwu6r8ejrss3axul8rxl" +
        "dph2q7z9kmrgvr7xlaqm47apw3d48zm203kzcq357a4ls9al2ea73r8jcceyjtya6fu5wzzpe50zrge6ulk" +
        "4nvjcpxlekvmxl6qcs9j3tz0469gqsjurz5"

    val fallbackAddr = LnTag.FallbackAddressTag(
      P2SHAddress.fromString("3EktnHQD7RiAE6uzMj2ZifT9YgRrkSgzQX"))

    val lnTags = LnTaggedFields(paymentHash = paymentTag,
                                descriptionOrHash = descpriptionHashTag,
                                fallbackAddress = Some(fallbackAddr))

    val signature = ECDigitalSignature.fromRS(
      "b6c6860fc6ff41bafba1745b538b6a7c6c2c0234f76bf817bf567be88cf2c632492c9dd279470841cd1e21a33ae7ed59b25809bf9b3366fe81881651589f5d15")
    val lnInvoiceSig =
      LnInvoiceSignature(signature = signature, recoverId = UInt8.zero)
    val lnInvoice = LnInvoice(hrp = hrpMilli,
                              timestamp = time,
                              lnTags = lnTags,
                              signature = lnInvoiceSig)

    val serialized = lnInvoice.toString

    lnInvoice.toString must be(expected)

    val deserialized = LnInvoice.fromStringT(serialized)

    deserialized.get.toString must be(serialized)
  }

  it must "parse BOLT11 example 7 (p2wpkh fallback addr)" in {
    //this test does not pass because bitcoin-s does not support p2wpkh currently

    val expected = "lnbc20m1pvjluez" +
      "pp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypq" +
      "hp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqs" +
      "fppqw508d6qejxtdg4y5r3zarvary0c5xw7kepvrhrm9s57hejg0p66" +
      "2ur5j5cr03890fa7k2pypgttmh4897d3raaq85a293e9jpuqwl0rnfu" +
      "wzam7yr8e690nd2ypcq9hlkdwdvycqe4x4ch"

    val fallbackAddr = LnTag.FallbackAddressTag(
      Bech32Address
        .fromString("bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"))

    val lnTags = LnTaggedFields(paymentHash = paymentTag,
                                descriptionOrHash = descpriptionHashTag,
                                fallbackAddress = Some(fallbackAddr))

    val signature = ECDigitalSignature.fromRS(
      "c8583b8f65853d7cc90f0eb4ae0e92a606f89caf4f7d65048142d7bbd4e5f3623ef407a75458e4b20f00efbc734f1c2eefc419f3a2be6d51038016ffb35cd613")

    val lnInvoiceSig =
      LnInvoiceSignature(signature = signature, recoverId = UInt8.zero)

    val lnInvoice = LnInvoice(hrp = hrpMilli,
                              timestamp = time,
                              lnTags = lnTags,
                              signature = lnInvoiceSig)

    val serialized = lnInvoice.toString

    serialized must be(expected)

    val deserialized = LnInvoice.fromStringT(serialized)

    deserialized.get must be(lnInvoice)
  }

  it must "parse BOLT11 example 8 (p2wsh fallback addr)" in {
    val expected = "lnbc20m1pvjluez" +
      "pp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypq" +
      "hp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqs" +
      "fp4qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3" +
      "q28j0v3rwgy9pvjnd48ee2pl8xrpxysd5g44td63g6xcjcu003j3qe8" +
      "878hluqlvl3km8rm92f5stamd3jw763n3hck0ct7p8wwj463cqm8cxgy"

    val fallbackAddr = LnTag.FallbackAddressTag(
      Bech32Address
        .fromString(
          "bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3"))

    val lnTags = LnTaggedFields(paymentHash = paymentTag,
                                descriptionOrHash = descpriptionHashTag,
                                fallbackAddress = Some(fallbackAddr))

    val signature = ECDigitalSignature.fromRS(
      "51e4f6446e410a164a6da9f39507e730c26241b4456ab6ea28d1b12c71ef8ca20c9cfe3dffc07d9f8db671ecaa4d20beedb193bda8ce37c59f85f82773a55d47")

    val lnInvoiceSig =
      LnInvoiceSignature(signature = signature, recoverId = UInt8.zero)

    val lnInvoice = LnInvoice(hrp = hrpMilli,
                              timestamp = time,
                              lnTags = lnTags,
                              signature = lnInvoiceSig)

    val serialized = lnInvoice.toString

    lnInvoice.toString must be(expected)

    val deserialized = LnInvoice.fromStringT(serialized)

    deserialized.get must be(lnInvoice)
  }

  it must "parse BOLT11 example 10" in {
    val expected =
      "lnbc25m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5vdhkven9v5sxyetpdeessp5zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zygs9q5sqqqqqqqqqqqqqqqpqqq4u9s93jtgysm3mrwll70zr697y3mf902hvxwej0v7c62rsltw83ng0pu8w3j230sluc5gxkdmm9dvpy9y6ggtjd2w544mzdrcs42t7sqdkcy8h"

    val signature = ECDigitalSignature.fromHex(
      "3045022100af0b02c64b4121b8ec6efffcf10f45f123b495eabb0cecc9ecf634a1c3eb71e30220343c3c3ba32545f0ff31441acddecad60485269085c9aa752b5d89a3c42aa5fa")
    val lnInvoiceSig =
      LnInvoiceSignature(recoverId = UInt8.zero, signature = signature)

    val descriptionTag = LnTag.DescriptionTag("coffee beans")

    val paymentSecret = Some(
      LnTag.SecretTag(PaymentSecret.fromHex(
        "1111111111111111111111111111111111111111111111111111111111111111")))

    val features = Some(
      LnTag.FeaturesTag(ByteVector.fromValidHex("800000000000000000000800")))

    val lnTags = LnTaggedFields(
      Vector(paymentTag, descriptionTag, paymentSecret.get, features.get))

    val hrpMilli =
      LnHumanReadablePart(LnBitcoinMainNet, Some(MilliBitcoins(25)))

    val lnInvoice = LnInvoice(hrp = hrpMilli,
                              timestamp = time,
                              lnTags = lnTags,
                              signature = lnInvoiceSig)

    val serialized = lnInvoice.toString
    // TODO uncomment when https://github.com/bitcoin-s/bitcoin-s/issues/1064 is fixed
    // serialized must be(expected)

    assert(serialized == expected)
    val deserialized = LnInvoice.fromStringT(serialized)

    deserialized.get.toString must be(serialized)
  }

  it must "deserialize and reserialize a invoice with a explicity expiry time" in {
    //from eclair
    val bech32 =
      "lnbcrt1m1pd6ssf3pp5mqcepx6yzx7uu0uagw5x3c7kqhnpwr3mfn844hjux8tlza6ztr7sdqqxqrrss0rl3gzer9gfc54fs84rd4xk6g8nf0syharnnyljc9za933memdzxrjz0v2v94ntuhdxduk3z0nlmpmznryvvvl4gzgu28kjkm4ey98gpmyhjfa"

    val invoiceT = LnInvoice.fromStringT(bech32)

    val deserialized = invoiceT.get.toString

    deserialized must be(bech32)
  }

  it must "have serialization symmetry for LnHrps" in {
    forAll(LnInvoiceGen.lnHrp) { hrp =>
      LnHumanReadablePart.fromString(hrp.toString) == hrp
    }
  }

  it must "have serialization symmetry for the invoices" in {

    forAll(LnInvoiceGen.lnInvoice) { invoice =>
      LnInvoice.fromStringT(invoice.toString).get == invoice

    }
  }

  it must "fail to create an invoice if the digital signature is invalid" in {
    intercept[IllegalArgumentException] {
      val sig = EmptyDigitalSignature
      val tags =
        LnTaggedFields(paymentHash = paymentTag,
                       descriptionOrHash =
                         Right(LnTag.DescriptionHashTag(descriptionHash)))
      val lnSig = LnInvoiceSignature(recoverId = UInt8.zero, signature = sig)
      LnInvoice(hrp = hrpEmpty,
                timestamp = UInt64.zero,
                lnTags = tags,
                signature = lnSig)
    }
  }

  it must "create a valid digital signature for an invoice" in {
    val privKey = ECPrivateKey.freshPrivateKey

    val tags =
      LnTaggedFields(paymentHash = paymentTag,
                     descriptionOrHash =
                       Right(LnTag.DescriptionHashTag(descriptionHash)))

    val invoice =
      LnInvoice.build(hrp = hrpEmpty, lnTags = tags, privateKey = privKey)

    assert(invoice.isValidSignature)
  }

  it must "handle the weird case if sigdata being exactly on a byte boundary, which means we need to pad the sigdata with a zero byte" in {
    //https://github.com/bitcoin-s/bitcoin-s-core/issues/277
    val expected =
      "03fad6c016f998e85d03ce0b7358b3b6a38ebc7fd60030340d0245fea0d95c8c12"
    val expectedHash =
      "6b80b9b7320bc1203534e78f86b6a32945c35ab464e475ed00e92c7b98755f9d"
    val str =
      "lntb100n1pwz34mzpp5dwqtndejp0qjqdf5u78cdd4r99zuxk45vnj8tmgqayk8hxr4t7wsd890v3xgatjv96xjmmwygarzvpsxqczcgnrdpskumn9ds3r5gn5wfskgetnygkzyetkv4h8gg36yfeh2cnnvdexjcn9ygkzyat4d9jzyw3zxqcrzvfjxgenxtf5xs6n2tfkxcmnwtfc8qunjttpv93xycmrv3jx2etxvc3zcgn90p3ksctwvajjyw3zvf5hgenfdejhsg3vyfehjmtzdakzyw3zgf2yx42ngs386xqrrssqr6xn7dtkyxk0rhl98k3esksst578uwhud5glp9svq24ddwlgqwz6v9uf7mqljrj07xl87ufrn4yfplrsz2vpmc9xwv44634h54dq3sq257hh4"
    val invoice = LnInvoice.fromStringT(str).get

    invoice.lnTags.paymentHash.hash.hex must be(expectedHash)

    invoice.signature.hex must be(
      "00f469f9abb10d678eff29ed1cc2d082e9e3f1d7e3688f84b0601556b5df401c2d30bc4fb60fc8727f8df3fb891cea4487e38094c0ef0533995aea35bd2ad04600")
    invoice.signature.signature.hex must be(
      "3044022000f469f9abb10d678eff29ed1cc2d082e9e3f1d7e3688f84b0601556b5df401c02202d30bc4fb60fc8727f8df3fb891cea4487e38094c0ef0533995aea35bd2ad046")

    invoice.lnTags.description.get.string must be(
      "{\"duration\":10000,\"channel\":\"trades\",\"event\":\"subscribe\",\"uuid\":\"00112233-4455-6677-8899-aabbccddeeff\",\"exchange\":\"bitfinex\",\"symbol\":\"BTCUSD\"}")

    invoice.timestamp must be(UInt64(1546180450))

    invoice.amount.get.toMSat must be(MilliSatoshis(10000))

    invoice.lnTags.expiryTime.get.u32 must be(UInt32(3600))

    invoice.isValidSignature must be(true)

    invoice.signatureData.toHex must be(
      "6c6e74623130306e0b851aec410d1ae02e6dcc82f0480d4d39e3e1ada8ca5170d6ad19391d7b403a4b1ee61d57e741a72bd91323ab930ba34b7b7111d1898181818161131b430b73732b6111d113a3930b232b991161132bb32b73a111d1139bab139b1b934b1329116113abab4b2111d111818189899191999969a1a1a9a969b1b1b9b969c1c1c9c96b0b0b13131b1b23232b2b33311161132bc31b430b733b2911d113134ba3334b732bc11161139bcb6b137b6111d11212a21aaa9a2113e8c018e100")

    invoice.toString must be(str)

    invoice.nodeId.hex must be(expected)
  }

  it must "parse secret and features tags" in {
    // generated by Eclair 3.3.0-SNAPSHOT
    val serialized =
      "lnbcrt10n1p0px7lfpp5ghc2y7ttnwy58jx0dfcsdxy7ey0qfryn0wcmm04ckud0qw73kt9sdq9vehk7xqrrss9qypqqqsp5qlf6efygd26y03y66jdqqfmlxthplnu5cc8648fgn88twhpyvmgqg9k5kd0k8vv3xvvqpkhkt9chdl579maq45gvck4g0yd0eggmvfkzgvjmwn29r99p57tgyl3l3s82hlc4e97at55xl5lyzpfk6n36yyqqxeem8q"
    val invoice = LnInvoice.fromStringT(serialized).get
    invoice.lnTags.secret must be(
      Some(LnTag.SecretTag(PaymentSecret.fromHex(
        "07d3aca4886ab447c49ad49a00277f32ee1fcf94c60faa9d2899ceb75c2466d0"))))
    invoice.lnTags.features must be(
      Some(LnTag.FeaturesTag(ByteVector.fromValidHex("0800"))))
    invoice.toString must be(serialized)
  }

  it must "ensure that the malleability of the checksum in bech32 strings cannot cause a signature to become valid" in {
    val strWithError =
      "lnbc2500u1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5xysxxatsyp3k7enxv4jsxqzpuaztrnwngzn3kdzw5hydlzf03qdgm2hdq27cqv3agm2awhz5se903vruatfhq77w3ls4evs3ch9zw97j25emudupq63nyw24cg27h2rspfj9srqqqqqp"

    assert(LnInvoice.fromStringT(strWithError).isFailure)
  }

  it must "parse unknown tags" in {
    val privateKeyHex =
      "180cb41c7c600be951b5d3d0a7334acc7506173875834f7a6c4c786a28fcbb19"
    val key: ECPrivateKey = ECPrivateKey(privateKeyHex)

    val unknownTag = LnTag.UnknownTag(
      LnTagPrefix.Unknown('z'),
      Bech32.from8bitTo5bit(ByteVector.fromValidHex("cafebabe")))

    val descriptionTag =
      LnTag.DescriptionTag("Please consider supporting this project")

    val tags =
      LnTaggedFields(Vector(paymentTag, descriptionTag, unknownTag))
    val expected = LnInvoice(hrpTestNetMilli, time, tags, key)
    val serialized = expected.toString
    val deserialized = LnInvoice.fromStringT(serialized).get
    deserialized must be(expected)
    deserialized.toString must be(serialized)
    deserialized.lnTags.tags.size must be(3)
    deserialized.lnTags.tags.last must be(unknownTag)
    deserialized.lnTags must be(expected.lnTags)
    deserialized.nodeId.bytes must be(key.publicKey.bytes)
  }

  it must "recover public keys" in {
    def testInvoice(str: String, nodeId: String): Unit = {
      val i = LnInvoice.fromStringT(str).get
      i.toString must be(str)
      i.nodeId must be(NodeId.fromHex(nodeId))
      ()
    }
    testInvoice(
      "lnbcrt500p1p0zk8umpp5wyc4s0h4jtu5lapsr4p2nevlpck7l5xec6rpjdv2a7r992vx0ctqdq9vehk7xqrrssfs6t6nyfutf4j8wzq6mf82lxefj5zadvw8fnjw6ev38y4578734zl94jfwnsfqdyt67da7g8shvhej0rkysymy260xyjtdv2dvhmvmgpdg6qjw",
      "03033ced5a027b2d1d0224f94cbf6983243f4ccbe07001c20b9ef2db3f116f82dc"
    )
    testInvoice(
      "lnbcrt1p0zk0pepp5f86agc2ue0lt5wvx96fczj9fhzy3swlassdrru7w23n7xq8zsnfqdq8w3jhxaqxqrrss2znyruaauwel7qu5ndrrydfpl9nrwk2lry8k898xguenakge0yrrdk37jcmvanv2dccmmkzhe9ncj0v84chpftrrravp52hyna8dm8qpegw8f8",
      "039c14dd6dbea913d3fa21b8aaa328cbacb9d6f1f967c3ead9a895c857958ed38a"
    )
  }
}
