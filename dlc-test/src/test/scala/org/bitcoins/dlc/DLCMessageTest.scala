package org.bitcoins.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  ContractInfo,
  DLCAccept,
  DLCOffer,
  OracleInfo
}
import org.bitcoins.commons.jsonmodels.dlc.{
  CETSignatures,
  DLCPublicKeys,
  DLCTimeouts
}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.BlockStamp.{BlockHeight, BlockTime}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.{
  DummyECDigitalSignature,
  ECPublicKey,
  Sha256DigestBE
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import scodec.bits.ByteVector

class DLCMessageTest extends BitcoinSAsyncTest {
  behavior of "DLCMessage"

  it must "not allow a DLCTimeout where the contract times out before it matures" in {
    assertThrows[IllegalArgumentException](
      DLCTimeouts(UInt32(5), BlockHeight(4), BlockHeight(2)))
    assertThrows[IllegalArgumentException](
      DLCTimeouts(UInt32(5), BlockTime(UInt32(4)), BlockTime(UInt32(2))))
  }

  val dummyPubKey: ECPublicKey = ECPublicKey.freshPublicKey
  val dummyPubKey2: ECPublicKey = ECPublicKey.freshPublicKey

  val dummyAddress: BitcoinAddress = BitcoinAddress(
    "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa").get

  val dummyHash: Sha256DigestBE = Sha256DigestBE(
    "00000000000000000008bba30d4d0fb53dcbffb601557de9f16d257d4f1985b7")

  val dummySig: PartialSignature =
    PartialSignature(dummyPubKey, DummyECDigitalSignature)

  it must "not allow a negative collateral for a DLCOffer" in {
    assertThrows[IllegalArgumentException](
      DLCOffer(
        ContractInfo.empty,
        OracleInfo.dummy,
        DLCPublicKeys(dummyPubKey, dummyPubKey2, dummyAddress),
        Satoshis(-1),
        Vector.empty,
        dummyAddress,
        SatoshisPerVirtualByte.one,
        DLCTimeouts(UInt32(5), BlockHeight(1), BlockHeight(2))
      ))
  }

  it must "not allow a negative collateral for a DLCAccept" in {
    assertThrows[IllegalArgumentException](
      DLCAccept(
        Satoshis(-1),
        DLCPublicKeys(dummyPubKey, dummyPubKey2, dummyAddress),
        Vector.empty,
        dummyAddress,
        CETSignatures(Map(dummyHash -> dummySig), dummySig),
        Sha256DigestBE(ByteVector.low(32))
      )
    )
  }

  it must "not allow duplicate keys in a DLCPublicKeys" in {
    assertThrows[IllegalArgumentException](
      DLCPublicKeys(dummyPubKey, dummyPubKey, dummyAddress)
    )
  }
}
