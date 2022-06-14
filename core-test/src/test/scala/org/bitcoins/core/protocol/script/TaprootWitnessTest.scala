package org.bitcoins.core.protocol.script

import org.bitcoins.crypto.{Sha256Digest, XOnlyPubKey}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class TaprootWitnessTest extends BitcoinSUnitTest {

  behavior of "TaprootWitness"

  //from this test case in script_assets.json with this comment 'tapscript/input80limit'
  //in script_assets.json
  val controlBlockHex =
    "c1a7957acbaaf7b444c53d9e0c9436e8a8a3247fd515095d66ddf6201918b40a3668f9a4ccdffcf778da624dca2dd" +
      "a0b08e763ec52fd4ad403ec7563a3504d0cc168b9a77a410029e01dac89567c9b2e6cd726e840351df3f2f58fefe976200a19244150d04153" +
      "909f660184d656ee95fa7bf8e1d4ec83da1fca34f64bc279b76d257ec623e08baba2cfa4ea9e99646e88f1eb1668c00c0f15b7443c8ab8348" +
      "1611cc3ae85eb89a7bfc40067eb1d2e6354a32426d0ce710e88bc4cc0718b99c325509c9d02a6a980d675a8969be10ee9bef82cafee2fc913" +
      "475667ccda37b1bc7f13f64e56c449c532658ba8481631c02ead979754c809584a875951619cec8fb040c33f06468ae0266cd8693d6a64cea" +
      "5912be32d8de95a6da6300b0c50fdcd6001ea41126e7b7e5280d455054a816560028f5ca53c9a50ee52f10e15c5337315bad1f5277acb109a" +
      "1418649dc6ead2fe14699742fee7182f2f15e54279c7d932ed2799d01d73c97e68bbc94d6f7f56ee0a80efd7c76e3169e10d1a1ba3b5f1eb0" +
      "2369dc43af687461c7a2a3344d13eb5485dca29a67f16b4cb988923060fd3b65d0f0352bb634bcc44f2fe668836dcd0f604150049835135dc" +
      "4b4fbf90fb334b3938a1f137eb32f047c65b85e6c1173b890b6d0162b48b186d1f1af8521945924ac8ac8efec321bf34f1d4b3d4a304a1031" +
      "3052c652d53f6ecb8a55586614e8950cde9ab6fe8e22802e93b3b9139112250b80ebc589aba231af535bb20f7eeec2e412f698c17f3fdc0a2" +
      "e20924a5e38b21a628a9e3b2a61e35958e60c7f5087c"

  val controlBlock = ControlBlock.fromHex(controlBlockHex)

  val merkleRootHex =
    "c5c62d7fc595ba5fbe61602eb1a29e2e4763408fe1e2b161beb7cb3c71ebcad9"

  val merkleRoot = Sha256Digest.fromHex(merkleRootHex)

  val tapLeafHash = Sha256Digest.fromHex(
    "8d76c657582b87b087f36579a9ea78816d7e2a94098bc3e3c6113ed4b6315bb4")

  val taprootSPK = TaprootScriptPubKey.fromHex(
    "2251201ebe8b90363bd097aa9f352c8b21914e1886bc09fe9e70c09f33ef2d2abdf4bc")

  val internalPubKey: XOnlyPubKey = XOnlyPubKey.fromHex(
    "a7957acbaaf7b444c53d9e0c9436e8a8a3247fd515095d66ddf6201918b40a36")
  it must "compute a tap leaf hash correctly" in {
    //from this test case in script_assets.json with this comment 'tapscript/input80limit'
    val expected =
      "8d76c657582b87b087f36579a9ea78816d7e2a94098bc3e3c6113ed4b6315bb4"
    val asmHex =
      "7520c7b5db9562078049719228db2ac80cb9643ec96c8055aa3b29c2c03d4d99edb0ac"
    val spk = ScriptPubKey.fromAsmHex(asmHex)
    assert(asmHex == spk.asmHex)
    val hash = TaprootScriptPath.computeTapleafHash(0xc0.toByte, spk)
    assert(hash.hex == expected)
  }

  it must "compute a merkle root for a tapscript tree" in {
    //expected from the 'compute a tap leaf hash correctly' test case referenced above

    val merkleRoot =
      TaprootScriptPath.computeTaprootMerkleRoot(controlBlock, tapLeafHash)
    assert(merkleRoot.hex == merkleRootHex)
  }

  it must "compute a tap tweak correctly" in {
    val expected =
      "4680a8777b651ac25f6da13f97f2bf530341b637da3f6295f2893e9d41b281aa"
    val tapTweakHash = internalPubKey.computeTapTweakHash(Some(merkleRoot))
    assert(tapTweakHash.hex == expected)
  }
  it must "verify a taproot commitment" in {
    val result =
      TaprootScriptPath.verifyTaprootCommitment(controlBlock = controlBlock,
                                                program = taprootSPK,
                                                tapLeafHash = tapLeafHash)
    assert(result)
  }

}
