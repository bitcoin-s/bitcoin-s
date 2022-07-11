package org.bitcoins.core.protocol.script

import org.bitcoins.crypto.{Sha256Digest, XOnlyPubKey}
import org.bitcoins.testkitcore.gen.WitnessGenerators
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.ByteVector

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

  it must "correctly detect an annex and compute its hash" in {
    val hex = Vector(
      "cd58cf65f98d25b89d2c5fb8e6300e1477a972dd045de513f34c84de85fb10c6466f00e373d087c2dcd6a9b9bc6abfd6d15dc4dc86dc51727d64683d477b4f25",
      "85ebd15b725b03998331eade85bdce98d1d54ee3a53f5c738f174bb619ecfec3ad0dc7",
      "752cd61855d62532f5944844b61339b008045bbb5eb89bc5b4b7903e48ab8da5d8832f07cecf9fd898eaff0b43925163676e567cba5788686ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead6ead587cba5987",
      "c018f50ed9dee3783c126dd1c0ea7a6e96b94deb13ccc90c0213616e7d4c3fa04214616b979f54093c806261f526162d563978f4dbfdce1998aa3c403017d67f1b2e71fda2d2096865d6f9d296e41d4c00d36db7a89a6e2ad899758e1529c4255eca996446a646bf6b4263c4903f0d28e2febf938bf0bf6142c86a4226c4935a846eae88fc335eed60ade78f5b74e7aaf4a143e6a791129d157b3670e578a46f74ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa53f50244b36f8778b8d47e08eb6e1fd3291f02da0bc4ba686f705158619c5efffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff82096544a4d0e53f476700387befc37ba1cecd281cfb7eee3571e698b58786cc78d0942890c656ececf4a7a55b04e7dd212521a13abd540fa58bc7f0df2293a79e0e60337607de6b37419cc03321ba0d0d0b53b31de7c143718b9d06ebe4332d04bdd682383e36a2028aae81f5acd79c147639b4f14b8e1641eb3e6aab379910851089c2eb1f856ea217ecb1bda29709d0bbf8c5df539074803904e9812621a3fcd7af2d6fa283efd72c962fc85fca8bf38513dda33c6bf7477ffeaa96a5d5b2ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc5be0d6cbb4e55173e4837d28cfbf7eda015507317c95aafc961ac61ddf1a24a11e001d486e1d21ae1b0e0967874eac28af6fd08e6d966fbae1066993d805e9cd47d1a814500af9e2a19475c3cd7aab4d14b5d3640c96caa7992efba7bafc12f0000000000000000000000000000000000000000000000000000000000000000dad5d0a6c9b4af217fc6bd63e8e081293b19fe971ef0c00a201fcf6af4f2e5b0ed1a1c592f0afd54fe098c6e94c0c126a691acef696fa0f321222da21d17333e262f0ab77b9b951c0cb6728cf634c157c3a014b90ebb7c1a4a2bce2676b8356ae4a6a7a677913e50591761048ddbd1703389169897ac38ec4bc22b3e32f1ba96d5942bb7b269c6649daa4d278aceb941990e749c8cbc4af67c0ea2bd24ec16090000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff18dd00d57a3965ec5da0e6f5a99ec2c5a4652d3ca5dfdf190cb30b70b26de11f51f4962874ea637a076692eeb88c7b46387a0e2de11a60ebe7869ef30d65aa8400000000000000000000000000000000000000000000000000000000000000006d3463c83cb06176593ca38b8f6651af7d9d0ccb582ef3ea8d606f07063c29e100000000000000000000000000000000000000000000000000000000000000006ce878752d89ec430e8267c1ec034bbd44ac468ceb9533b81c694ae96136efac8588dad32cf38cd0d30406eb38c3bb9eb370705927f2e36e9fc8b2a15680f7fd23f61e2ab670d37183b270d328412d32b4bfa9366baa9628a61125f814cdd92de3451037bc1e2c4b7d320e8e2f849cf43611dc0e4eafa932e0daecd053f12d978f1d596ff6864f2e15dbb05e92671fdf642ee79311a4e3007f6fe4ecbf25be68b35cffb8daa8de56366879bcbdee727ca9087c8d5fcfa4aff4d6a2cc8624bedae7b9010fa5b5e676139448ecda567903356326588bd4ffb147fe93fb2527716300000000000000000000000000000000000000000000000000000000000000003abdd0bf57ba6d970677346ea9e1d6498ca889cd54150b4a2d9226b3c87990938ca3770cea5395a7e3d48d5264e44fa870418bd876c5380e0c32a6277a7d1ce9a1d9d29b5281add5c25f90cb7926cf03d7700088e503d522ad8b096a685165c3dc4a8d3cf0276a4a639fae4a6660bf64898465886efbbf7f97cb3bed2fbf360ccdc059a753ac18e46fa6629aaa64f8bbed3ab7985c2ea9f23e453a1e90f59495c5f5e9dc3db7a2d09d8f8ced75259d36ad068ef38eeddf1a96e454cb080ddffd",
      "503bd07ecabe591664d9eaac4218fc1d079126033a09e6596a01778f67354f788b12777f96b42168299d453dc6eca16e3d48d7804725926c0dc735622e2a3f7fb6d7f3ac156537b9e649bf0313698d20597713d1fb11601eb658b76076ee44f3878d7ac6d984c8928bfeaac518d758e5d3affd75978f179baeb90aa8440bc7a48e0937fae42cafef7669c5cdbf0d439e511d829466debc8994b68d6f7914536f2fdc7ca88b3fd396c2da5992e9e507a74739e5bc9534000f281fde16b6a2f5e3b03aac139099536ff52649b4a7da13c4148afa7af1831f877f7b951d60a0812f790bf5739d84ef9c3e8a8655ec71237a3c93f10115d63ed2d01572991973baf6a1ef50b3f8d1d465ca54356620c5171a21054ab3bb61d349db6fbdb2b4d6e1e94ae3415fb780a8a7df4cea3ba9d9caa15c275962e7ffc6634436ba0296f7c61be9b98b19857c6f0750d366ff8f7b39fd09f2794e5414032fd93b0056326c156be4cd842c7ae446c57fa1ed6dc83e257bdf5027cf9e39011cd76c01bd1c5a57082b214e347764594752790eb649cd8ac0a3a52f47aad28254dfdb72253fd957d46ad4158daa16cd9f24add4faf73a2bd26417ebfa937681d534614f8e089d2d0aff5815ba2db4018dac77dbaa32da3ecd51e5eb05e398146d17a5db2c7470bd0b85fb5717dfc9a259b4f2e147ea666aa939acb316d5da5a9fa56f129b03dad2ba58627de562d0f04ea98c7b722130052e2dd435b0b0a6586fba4f48a5df53d6e046e80d1a11"
    )
    val stack = hex.map(h => ByteVector.fromValidHex(h))
    val taprootScriptPath = TaprootScriptPath.fromStack(stack.reverse)
    assert(taprootScriptPath.annexOpt.isDefined)
    assert(
      taprootScriptPath.annexOpt.get.toHex == "503bd07ecabe591664d9eaac4218fc1d079126033a09e6596a01778f67354f788b12777f96b42168299d453dc6eca16e3d48d7804725926c0dc735622e2a3f7fb6d7f3ac156537b9e649bf0313698d20597713d1fb11601eb658b76076ee44f3878d7ac6d984c8928bfeaac518d758e5d3affd75978f179baeb90aa8440bc7a48e0937fae42cafef7669c5cdbf0d439e511d829466debc8994b68d6f7914536f2fdc7ca88b3fd396c2da5992e9e507a74739e5bc9534000f281fde16b6a2f5e3b03aac139099536ff52649b4a7da13c4148afa7af1831f877f7b951d60a0812f790bf5739d84ef9c3e8a8655ec71237a3c93f10115d63ed2d01572991973baf6a1ef50b3f8d1d465ca54356620c5171a21054ab3bb61d349db6fbdb2b4d6e1e94ae3415fb780a8a7df4cea3ba9d9caa15c275962e7ffc6634436ba0296f7c61be9b98b19857c6f0750d366ff8f7b39fd09f2794e5414032fd93b0056326c156be4cd842c7ae446c57fa1ed6dc83e257bdf5027cf9e39011cd76c01bd1c5a57082b214e347764594752790eb649cd8ac0a3a52f47aad28254dfdb72253fd957d46ad4158daa16cd9f24add4faf73a2bd26417ebfa937681d534614f8e089d2d0aff5815ba2db4018dac77dbaa32da3ecd51e5eb05e398146d17a5db2c7470bd0b85fb5717dfc9a259b4f2e147ea666aa939acb316d5da5a9fa56f129b03dad2ba58627de562d0f04ea98c7b722130052e2dd435b0b0a6586fba4f48a5df53d6e046e80d1a11")
    val expectedAnnexHash = {
      "1c0aaaf600682c151456bc3c56b49f1a28afd49344c04d88393fe589106ec22e"
    }
    assert(taprootScriptPath.annexHashOpt.get.hex == expectedAnnexHash)
  }

  it must "construct a taproot keypath witness with an annex" in {
    val vec = Vector(
      "c2bdc23435c7bbdce741081181eecd31865f7d94fad6c49c8b1f4619aad72b83354530dbc9446243ff81e0dac2e77b2d437b9d53d279b535a23fb8c599454b3e02",
      "50ba")
    val stack = vec.map(ByteVector.fromValidHex(_))
    val tr = TaprootWitness.fromStack(stack.reverse)
    assert(tr.isInstanceOf[TaprootKeyPath])
    assert(tr.annexOpt == Some(stack.last))
  }

  it must "construct a taproot keypath with a leading byte 0x50 but does NOT have an annex" in {
    val hex =
      "50795800afc8005c6d57ddb994ffa8d0e343549b4abd2ce38671cf14c92769091deee4eaa7704e84536f8e0de52789c8cc8e679d21c4ec060f5d92d51ed9562e"
    val stack = Vector(hex).map(ByteVector.fromValidHex(_))
    val witness = ScriptWitness(stack)
    assert(witness.isInstanceOf[TaprootKeyPath])
  }

  it must "have serialization symmetry" in {
    forAll(WitnessGenerators.taprootWitness) { wit =>
      val fromBytes = TaprootWitness.fromBytes(wit.bytes)
      assert(fromBytes == wit)
      assert(TaprootWitness.fromStack(wit.stack.toVector) == wit)
      assert(fromBytes.bytes == wit.bytes)
    }
  }
}
