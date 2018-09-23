package org.bitcoins.core.protocol.ln

import org.bitcoins.core.crypto.{ ECDigitalSignature, ECPublicKey, Sha256Digest }
import org.bitcoins.core.number.{ UInt32, UInt64, UInt8 }
import org.bitcoins.core.protocol.{ Bech32Address, P2PKHAddress, P2SHAddress }
import org.bitcoins.core.protocol.ln.LnParams.{ LnBitcoinMainNet, LnBitcoinTestNet }
import org.bitcoins.core.protocol.ln.fee.{ FeeBaseMSat, FeeProportionalMillionths }
import org.bitcoins.core.protocol.ln.routing.LnRoute
import org.bitcoins.core.util.CryptoUtil
import org.scalatest.{ FlatSpec, MustMatchers }
import scodec.bits.ByteVector

class LnInvoiceUnitTest extends FlatSpec with MustMatchers {
  behavior of "LnInvoice"

  val hrpEmpty = LnHumanReadablePart(LnBitcoinMainNet)
  val hrpMicro = LnHumanReadablePart(LnBitcoinMainNet, Some(MicroBitcoins(2500)))
  val hrpMilli = LnHumanReadablePart(LnBitcoinMainNet, Some(MilliBitcoins(20)))
  val hrpTestNetMilli = LnHumanReadablePart(LnBitcoinTestNet, Some(MilliBitcoins(20)))
  val time = UInt64(1496314658)

  val paymentHash = Sha256Digest.fromHex("0001020304050607080900010203040506070809000102030405060708090102")
  val paymentTag = LnInvoiceTag.PaymentHashTag(paymentHash)

  val description = {
    ("One piece of chocolate cake, one icecream cone, one pickle, one slice of swiss cheese, " +
      "one slice of salami, one lollypop, one piece of cherry pie, one sausage, one cupcake, " +
      "and one slice of watermelon").getBytes()
  }
  val descriptionHash = CryptoUtil.sha256(ByteVector(description))

  val descpriptionHashTag = Right(LnInvoiceTag.DescriptionHashTag(descriptionHash))

  it must "parse BOLT11 example 1" in {
    //BOLT11 Example #1

    val descriptionTagE = Left(LnInvoiceTag.DescriptionTag("Please consider supporting this project"))
    val lnTags = LnInvoiceTaggedFields(
      paymentHash = paymentTag,
      descriptionOrHash = descriptionTagE)

    val signature = ECDigitalSignature.fromRS("38ec6891345e204145be8a3a99de38e98a39d6a569434e1845c8af7205afcfcc7f425fcd1463e93c32881ead0d6e356d467ec8c02553f9aab15e5738b11f127f")
    val version = UInt8.zero
    val lnSig = LnInvoiceSignature(version, signature)

    val invoice = Invoice(hrpEmpty, time, lnTags, lnSig)

    val serialized = invoice.toString
    serialized must be("lnbc1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpl2pkx2ctnv5sxxmmwwd5kgetjypeh2ursdae8g6twvus8g6rfwvs8qun0dfjkxaq8rkx3yf5tcsyz3d73gafnh3cax9rn449d9p5uxz9ezhhypd0elx87sjle52x86fux2ypatgddc6k63n7erqz25le42c4u4ecky03ylcqca784w")

    val deserialized = LnInvoice.fromString(serialized)

    deserialized.get.toString must be(invoice.toString)
  }

  it must "parse BOLT11 example 2" in {
    //BOLT11 Example #2

    val descriptionTagE = Left(LnInvoiceTag.DescriptionTag("1 cup coffee"))
    val expiryTimeTag = LnInvoiceTag.ExpiryTimeTag(UInt32(60))
    val lnTags = LnInvoiceTaggedFields(
      paymentTag,
      descriptionOrHash = descriptionTagE,
      expiryTime = Some(expiryTimeTag))

    val signature = ECDigitalSignature.fromRS("e89639ba6814e36689d4b91bf125f10351b55da057b00647a8dabaeb8a90c95f160f9d5a6e0f79d1fc2b964238b944e2fa4aa677c6f020d466472ab842bd750e")
    val version = UInt8.one
    val lnSig = LnInvoiceSignature(version, signature)

    val invoice = Invoice(hrpMicro, time, lnTags, lnSig)

    val serialized = invoice.toString

    serialized must be("lnbc2500u1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5xysxxatsyp3k7enxv4jsxqzpuaztrnwngzn3kdzw5hydlzf03qdgm2hdq27cqv3agm2awhz5se903vruatfhq77w3ls4evs3ch9zw97j25emudupq63nyw24cg27h2rspfj9srp")

    val deserialized = LnInvoice.fromString(serialized)

    deserialized.get.toString must be(invoice.toString)
  }

  it must "parse BOLT11 example 3" in {
    //BOLT11 Example #3 - Description field does not encode correctly due to Japanese letters

    val descriptionTagE = Left(LnInvoiceTag.DescriptionTag("ナンセンス 1杯"))
    val expiryTag = LnInvoiceTag.ExpiryTimeTag(UInt32(60))
    val lnTags = LnInvoiceTaggedFields(
      paymentTag, descriptionTagE, None,
      Some(expiryTag), None, None,
      None)

    val signature = ECDigitalSignature.fromRS("259f04511e7ef2aa77f6ff04d51b4ae9209504843e5ab9672ce32a153681f687515b73ce57ee309db588a10eb8e41b5a2d2bc17144ddf398033faa49ffe95ae6")
    val version = UInt8.zero
    val lnSig = LnInvoiceSignature(version, signature)

    val invoice = Invoice(hrpMicro, time, lnTags, lnSig)

    val serialized = invoice.toString

    invoice.toString must be("lnbc2500u1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpquwpc4curk03c9wlrswe78q4eyqc7d8d0xqzpuyk0sg5g70me25alkluzd2x62aysf2pyy8edtjeevuv4p2d5p76r4zkmneet7uvyakky2zr4cusd45tftc9c5fh0nnqpnl2jfll544esqchsrny")

    val deserialized = LnInvoice.fromString(serialized)

    deserialized.get must be(invoice)
  }

  it must "parse BOLT11 example 4" in {
    //BOLT11 Example #4

    val descriptionHash = Sha256Digest.fromHex("3925b6f67e2c340036ed12093dd44e0368df1b6ea26c53dbe4811f58fd5db8c1")
    val descriptionHashTagE = Right(LnInvoiceTag.DescriptionHashTag(descriptionHash))
    val lnTags = LnInvoiceTaggedFields(
      paymentHash = paymentTag,
      descriptionOrHash = descriptionHashTagE,
      None, None, None,
      None, None)

    val signature = ECDigitalSignature.fromRS("c63486e81f8c878a105bc9d959af1973854c4dc552c4f0e0e0c7389603d6bdc67707bf6be992a8ce7bf50016bb41d8a9b5358652c4960445a170d049ced4558c")
    val version = UInt8.zero
    val lnSig = LnInvoiceSignature(version, signature)

    val invoice = Invoice(hrpMilli, time, lnTags, lnSig)

    val serialized = invoice.toString

    invoice.toString must be("lnbc20m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqscc6gd6ql3jrc5yzme8v4ntcewwz5cnw92tz0pc8qcuufvq7khhr8wpald05e92xw006sq94mg8v2ndf4sefvf9sygkshp5zfem29trqq2yxxz7")

    val deserialized = LnInvoice.fromString(serialized)

    deserialized.get must be(invoice)
  }

  it must "parse BOLT11 example 5" in {
    //BOLT11 Example #5

    val descriptionHash = Sha256Digest.fromHex("3925b6f67e2c340036ed12093dd44e0368df1b6ea26c53dbe4811f58fd5db8c1")
    val descriptionHashTagE = Right(LnInvoiceTag.DescriptionHashTag(descriptionHash))
    val fallbackAddr = LnInvoiceTag.FallbackAddressTag(P2PKHAddress.fromString("mk2QpYatsKicvFVuTAQLBryyccRXMUaGHP").get)

    val lnTags = LnInvoiceTaggedFields(
      paymentHash = paymentTag,
      descriptionOrHash = descriptionHashTagE,
      fallbackAddress = Some(fallbackAddr))

    val signature = ECDigitalSignature.fromRS("b6c42b8a61e0dc5823ea63e76ff148ab5f6c86f45f9722af0069c7934daff70d5e315893300774c897995e3a7476c8193693d144a36e2645a0851e6ebafc9d0a")
    val version = UInt8.one
    val lnSig = LnInvoiceSignature(version, signature)

    val invoice = Invoice(hrpTestNetMilli, time, lnTags, lnSig)

    val serialized = invoice.toString

    serialized must be("lntb20m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqsfpp3x9et2e20v6pu37c5d9vax37wxq72un98kmzzhznpurw9sgl2v0nklu2g4d0keph5t7tj9tcqd8rexnd07ux4uv2cjvcqwaxgj7v4uwn5wmypjd5n69z2xm3xgksg28nwht7f6zsp2mh7qm")
    //In example #5, the order in which tags are encoded in the invoice has been changed to demonstrate the ability to move tags as needed.
    //For that reason, the example #5 output we are matching against has been modified to fit the order in which we encode our invoices.
    //TODO: Add checksum data to check

    val deserialized = LnInvoice.fromString(serialized)

    deserialized.get.toString must be(serialized)
  }
  it must "parse BOLT11 example 6" in {
    val expected = "lnbc20m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqsfpp3qjmp7lwpagxun9pygexvgpjdc4jdj85fr9yq20q82gphp2nflc7jtzrcazrra7wwgzxqc8u7754cdlpfrmccae92qgzqvzq2ps8pqqqqqqpqqqqq9qqqvpeuqafqxu92d8lr6fvg0r5gv0heeeqgcrqlnm6jhphu9y00rrhy4grqszsvpcgpy9qqqqqqgqqqqq7qqzqj9n4evl6mr5aj9f58zp6fyjzup6ywn3x6sk8akg5v4tgn2q8g4fhx05wf6juaxu9760yp46454gpg5mtzgerlzezqcqvjnhjh8z3g2qqdhhwkj"

    val fallbackAddr = LnInvoiceTag.FallbackAddressTag(P2PKHAddress.fromString("1RustyRX2oai4EYYDpQGWvEL62BBGqN9T").get)

    val signature = ECDigitalSignature.fromRS(
      "91675cb3fad8e9d915343883a49242e074474e26d42c7ed914655689a8074553733e8e4ea5ce9b85f69e40d755a55014536b12323f8b220600c94ef2b9c51428")
    val lnInvoiceSig = LnInvoiceSignature(
      version = UInt8.zero,
      signature = signature)

    val route1 = LnRoute(
      pubkey = ECPublicKey.fromHex("029e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255"),
      shortChannelID = ShortChannelId.fromHex("0102030405060708"),
      feeBaseMsat = FeeBaseMSat(PicoBitcoins.one),
      feePropMilli = FeeProportionalMillionths(UInt32(20)),
      cltvExpiryDelta = 3)

    val route2 = LnRoute(
      pubkey = ECPublicKey.fromHex("039e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255"),
      shortChannelID = ShortChannelId.fromHex("030405060708090a"),
      feeBaseMsat = FeeBaseMSat(PicoBitcoins(2)),
      feePropMilli = FeeProportionalMillionths(UInt32(30)),
      cltvExpiryDelta = 4)

    val route = LnInvoiceTag.RoutingInfo(Vector(route1, route2))

    val lnTags = LnInvoiceTaggedFields(
      paymentHash = paymentTag,
      descriptionOrHash = descpriptionHashTag,
      fallbackAddress = Some(fallbackAddr),
      routingInfo = Some(route))

    val lnInvoice = Invoice(
      hrp = hrpMilli,
      timestamp = time,
      lnTags = lnTags,
      signature = lnInvoiceSig)

    val serialized = lnInvoice.toString
    serialized must be(expected)

    val deserialized = LnInvoice.fromString(serialized)

    deserialized.get.toString must be(serialized)
  }

  it must "parse BOLT11 example 7 (p2sh fallback addr)" in {
    val expected = "lnbc20m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypq" +
      "hp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqsfppj3a24vwu6r8ejrss3axul8rxl" +
      "dph2q7z9kmrgvr7xlaqm47apw3d48zm203kzcq357a4ls9al2ea73r8jcceyjtya6fu5wzzpe50zrge6ulk" +
      "4nvjcpxlekvmxl6qcs9j3tz0469gqsjurz5"

    val fallbackAddr = LnInvoiceTag.FallbackAddressTag(P2SHAddress.fromString("3EktnHQD7RiAE6uzMj2ZifT9YgRrkSgzQX").get)

    val lnTags = LnInvoiceTaggedFields(
      paymentHash = paymentTag,
      descriptionOrHash = descpriptionHashTag,
      fallbackAddress = Some(fallbackAddr))

    val signature = ECDigitalSignature.fromRS("b6c6860fc6ff41bafba1745b538b6a7c6c2c0234f76bf817bf567be88cf2c632492c9dd279470841cd1e21a33ae7ed59b25809bf9b3366fe81881651589f5d15")
    val lnInvoiceSig = LnInvoiceSignature(
      signature = signature,
      version = UInt8.zero)
    val lnInvoice = Invoice(
      hrp = hrpMilli,
      timestamp = time,
      lnTags = lnTags,
      signature = lnInvoiceSig)

    val serialized = lnInvoice.toString

    lnInvoice.toString must be(expected)

    val deserialized = LnInvoice.fromString(serialized)

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

    val fallbackAddr = LnInvoiceTag.FallbackAddressTag(Bech32Address.fromString("bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4").get)

    val lnTags = LnInvoiceTaggedFields(
      paymentHash = paymentTag,
      descriptionOrHash = descpriptionHashTag,
      fallbackAddress = Some(fallbackAddr))

    val signature = ECDigitalSignature.fromRS("c8583b8f65853d7cc90f0eb4ae0e92a606f89caf4f7d65048142d7bbd4e5f3623ef407a75458e4b20f00efbc734f1c2eefc419f3a2be6d51038016ffb35cd613")

    val lnInvoiceSig = LnInvoiceSignature(
      signature = signature,
      version = UInt8.zero)

    val lnInvoice = Invoice(
      hrp = hrpMilli,
      timestamp = time,
      lnTags = lnTags,
      signature = lnInvoiceSig)

    val serialized = lnInvoice.toString

    serialized must be(expected)

    val deserialized = LnInvoice.fromString(serialized)

    deserialized.get must be(lnInvoice)
  }

  it must "parse BOLT11 example 8 (p2wsh fallback addr)" in {
    val expected = "lnbc20m1pvjluez" +
      "pp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypq" +
      "hp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqs" +
      "fp4qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3" +
      "q28j0v3rwgy9pvjnd48ee2pl8xrpxysd5g44td63g6xcjcu003j3qe8" +
      "878hluqlvl3km8rm92f5stamd3jw763n3hck0ct7p8wwj463cqm8cxgy"

    val fallbackAddr = LnInvoiceTag.FallbackAddressTag(Bech32Address.fromString("bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3").get)

    val lnTags = LnInvoiceTaggedFields(
      paymentHash = paymentTag,
      descriptionOrHash = descpriptionHashTag,
      fallbackAddress = Some(fallbackAddr))

    val signature = ECDigitalSignature.fromRS("51e4f6446e410a164a6da9f39507e730c26241b4456ab6ea28d1b12c71ef8ca20c9cfe3dffc07d9f8db671ecaa4d20beedb193bda8ce37c59f85f82773a55d47")

    val lnInvoiceSig = LnInvoiceSignature(
      signature = signature,
      version = UInt8.zero)

    val lnInvoice = Invoice(
      hrp = hrpMilli,
      timestamp = time,
      lnTags = lnTags,
      signature = lnInvoiceSig)

    val serialized = lnInvoice.toString

    lnInvoice.toString must be(expected)

    val deserialized = LnInvoice.fromString(serialized)

    deserialized.get must be(lnInvoice)
  }
}