package org.bitcoins.commons.dlc

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
  ECAdaptorSignature,
  ECPublicKey,
  Sha256DigestBE
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import scodec.bits.ByteVector

class DLCMessageTest extends BitcoinSAsyncTest {
  behavior of "DLCMessage"

  it must "not allow a DLCTimeout where the contract times out before it matures" in {
    assertThrows[IllegalArgumentException](
      DLCTimeouts(BlockHeight(4), BlockHeight(2)))
    assertThrows[IllegalArgumentException](
      DLCTimeouts(BlockTime(UInt32(4)), BlockTime(UInt32(2))))
  }

  val dummyPubKey: ECPublicKey = ECPublicKey.freshPublicKey
  val dummyPubKey2: ECPublicKey = ECPublicKey.freshPublicKey

  val dummyAddress: BitcoinAddress = BitcoinAddress(
    "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa")

  val dummyHash: Sha256DigestBE = Sha256DigestBE(
    "00000000000000000008bba30d4d0fb53dcbffb601557de9f16d257d4f1985b7")

  val dummySig: PartialSignature =
    PartialSignature(dummyPubKey, DummyECDigitalSignature)

  it must "not allow a negative collateral for a DLCOffer" in {
    assertThrows[IllegalArgumentException](
      DLCOffer(
        ContractInfo.empty,
        OracleInfo.dummy,
        DLCPublicKeys(dummyPubKey, dummyAddress),
        Satoshis(-1),
        Vector.empty,
        dummyAddress,
        SatoshisPerVirtualByte.one,
        DLCTimeouts(BlockHeight(1), BlockHeight(2))
      ))
  }

  it must "not allow a negative collateral for a DLCAccept" in {
    assertThrows[IllegalArgumentException](
      DLCAccept(
        Satoshis(-1),
        DLCPublicKeys(dummyPubKey, dummyAddress),
        Vector.empty,
        dummyAddress,
        CETSignatures(Map(dummyHash -> ECAdaptorSignature.dummy), dummySig),
        Sha256DigestBE(ByteVector.low(32))
      )
    )
  }
}
