package org.bitcoins.dlc.oracle

import org.bitcoins.core.api.dlcoracle.OracleEvent
import org.bitcoins.core.protocol.dlc.SigningVersion
import org.bitcoins.core.protocol.tlv.{
  OracleAnnouncementV0TLV,
  OracleAttestmentV0TLV
}
import org.bitcoins.crypto.FieldElement
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class AttestationVerificationTest extends BitcoinSUnitTest {

  behavior of "AttestationVerification"

  val signingVersion: SigningVersion = SigningVersion.DLCOracleV0SigningVersion

  val enumAnnouncement: OracleAnnouncementV0TLV =
    OracleAnnouncementV0TLV(
      "fdd8249917caf0b7f6cd8be0cd7a98530464329e42219e3b0f0cd75609de1d7342e8eba7ab47f111e1825af3322fd4d97f6d1b5f5063394f9a2dae05c14567a1f10819fa545aa0024da81c3fec63e56e07ee141cbefbd2c6e7d4dede124fe856ea453a85fdd8223500013168cb6d4c4e52aeb5bb75ce141cd9e1aa40e1d9123134d9aa390cffb338d51e600a1580fdd806060002016101620474657374")

  val validEnumAttestation: OracleAttestmentV0TLV = OracleAttestmentV0TLV(
    "fdd868690474657374545aa0024da81c3fec63e56e07ee141cbefbd2c6e7d4dede124fe856ea453a8500013168cb6d4c4e52aeb5bb75ce141cd9e1aa40e1d9123134d9aa390cffb338d51e323d991dadb52f32d0541027f973c363c0b746bb40dd1d42686f172d88ddef380161")

  val invalidEnumAttestation: OracleAttestmentV0TLV =
    validEnumAttestation.copy(sigs =
      validEnumAttestation.sigs.map(_.copy(sig = FieldElement.one)))

  val unsignedDigitDecompAnnouncement: OracleAnnouncementV0TLV =
    OracleAnnouncementV0TLV(
      "fdd824fd0143e3335c998ff6b188da3ee303b8775f64ca7810685a4051fbf1acd1181f4dbcbc15ac5b418e37d9600229e5c31b97d130ceb9130d163c9592bf0481956dde0d64545aa0024da81c3fec63e56e07ee141cbefbd2c6e7d4dede124fe856ea453a85fdd822df0006280b657b9c1cd8de3da2619194e2c71831598be3e60d39a242c232f580451c43bd61d1a6c395c99202058ddabf851e1c8220f12bd801bbb90efcf45d4a2d769c57ae1c605b9d36ff8477baa8c216abbfe6c3742236ecfad2415745a7cf7850c646f4835c5b80ca64832a6b9e7526fd575db4913ce0a9686072c5f970f94c3a2e87d4b2e93e52b2283c81185fb8ab14a757ff04b00b821d1aaac0a81e05d15da64ca823f9aa4b18525521073096f73fd583205086c7db2b6ac243901e4f1898bc600b6700fdd80a0f00020005756e6974730000000000060564756d6d79")

  val validUnsignedDigitDecompAttestation: OracleAttestmentV0TLV =
    OracleAttestmentV0TLV(
      "fdd868fd01b40564756d6d79545aa0024da81c3fec63e56e07ee141cbefbd2c6e7d4dede124fe856ea453a850006280b657b9c1cd8de3da2619194e2c71831598be3e60d39a242c232f580451c43050ce8ac95b549bb5fed9e3800cf3c040207071032a5c458485ad1817373c0b7bd61d1a6c395c99202058ddabf851e1c8220f12bd801bbb90efcf45d4a2d769cdaa4e883da6b59cac21fc8f18dae7893997d5d13ac63f33fdb7643bba4c4fc8d57ae1c605b9d36ff8477baa8c216abbfe6c3742236ecfad2415745a7cf7850c6cb6a147a2e2a8875133147055027ed130dec47c6a9f75983d532a5c5940a763546f4835c5b80ca64832a6b9e7526fd575db4913ce0a9686072c5f970f94c3a2e72d0655c541eac15e9caa5af059c1f3e433507f1782e8775c555f1c402509f0c87d4b2e93e52b2283c81185fb8ab14a757ff04b00b821d1aaac0a81e05d15da68e710b25a91bfdacf179d9da3b90dec98844d8ac1ed534922dfa362b9db86c134ca823f9aa4b18525521073096f73fd583205086c7db2b6ac243901e4f1898bc476a311fbcd5d2fb7a355a9032c67dead084fe66eed00f7e9646e6fa83902bc7013001300130013001300130")

  // this one was generated with the same public key
  val invalidUnsignedDigitDecompAttestation: OracleAttestmentV0TLV =
    validUnsignedDigitDecompAttestation.copy(sigs =
      validUnsignedDigitDecompAttestation.sigs.map(
        _.copy(sig = FieldElement.one)))

  // this one was generated with a different public key
  val invalidUnsignedDigitDecompAttestation1: OracleAttestmentV0TLV =
    OracleAttestmentV0TLV(
      "fdd868fd01b00161d485e5557110ec2a4909438ce55f02d9ec349b33989c6344fbbd19aec4ae56d8000667aabd3d20e82e2975d8629240f77295e8d76985f525450372763db775ea0f9029aa9bfb01cc81a39849e81f6d96057e528dcf4decb40a5a521bed2f580871bea09840a976ae6bb2a4052b955bf4720f8178cabe2cd95e490d4853f5772c40060c3a6ed993e7104ceb951a222cd14d0fac69d26ec00d07c3c7de65be07d10fdf994e8ea291c438d11067b84a859e73d99ee7030a246eaaeb8573ad2d7fc1844b2afca351f6dd0c8171090196dfc8fa9641e0748902bcbad6270b36179252fc31ce5c536d7cb69d509f583aa1bd09aac5e7ebbd65872e2809176f55af9f810514a82a8a51b5306ef586aabe8789d3cc47972124a4da4834a2507d1315b7893d5ecb7fd7d4d4bd27eb7c9ce7ff92080b9959625cb4d2a92ac1a0d487168402dfd23248c8f9bce7963f097e58ca5412db68b2f9f9bca86bfe35d83fcedbafb9edccb363f7788fa111bdd9ba8558e7e1c921f04144e24d03a98a81567301df4e511c4ffbbc691beb13498b6628817915ecfb41e5f1e63317d646920df10f4ae9797c013001300130013001300130")

  val signedDigitDecompAnnouncement: OracleAnnouncementV0TLV =
    OracleAnnouncementV0TLV(
      "fdd824fd01661aabd9bbcae0207aff9510f05099295f32ff72290cf5c494d3869f582f8c4a6cf1b7d832562047f68ce607eb39dcd7ec8ce64432dc51a8853dc5a3acd96a8bc5545aa0024da81c3fec63e56e07ee141cbefbd2c6e7d4dede124fe856ea453a85fdd822fd010000070652285e89487dc8ce816a81234082394d9602263d35a7322b77299082257767b559c622def4bba15a2ad7fc336edd71ace9b4c366b9eaba22b73df00589e74b7ca5b6c7301ef7dc62aeae1823018107868d8956677421e11ffd8f125f2fedf4a527003355640ee9333cda6c37a92d4989c6ab96eddc9266f0ddce0e2a3ffb77aa9eefa1fe40eddd0fa63f501e9b368eed6ab0cc0d2e5e6da1baa570ed9e857134bbc8a15dd5949eb1203b1d15ae701fe4b04707a1ea54c10fef16308bf806f2aa0b17f8673fe785f6b9ff0718e55b621c8e9d92839759a98b88bd6590a0ff856011fe80fdd80a0f00020105756e697473000000000006067369676e6564")

  val validSignedDigitDecompAttestation: OracleAttestmentV0TLV =
    OracleAttestmentV0TLV(
      "fdd868fd01f7067369676e6564545aa0024da81c3fec63e56e07ee141cbefbd2c6e7d4dede124fe856ea453a8500070652285e89487dc8ce816a81234082394d9602263d35a7322b772990822577670ab288b31d99f56d18d4f34be875c0a4d73aae135c4f50349a1014b686d69841b559c622def4bba15a2ad7fc336edd71ace9b4c366b9eaba22b73df00589e74ba7b82eef2041bf6af1511016cadabe9d52e64d875caf5bfef85903dbc4fc00737ca5b6c7301ef7dc62aeae1823018107868d8956677421e11ffd8f125f2fedf469f40edbb7846274f46a973f5442ece91b5a6e450a8cdcef272058a27176dabba527003355640ee9333cda6c37a92d4989c6ab96eddc9266f0ddce0e2a3ffb7762f10e09f79273ab04d1549c1d60054738fe903575aa732760bd2668530459e9aa9eefa1fe40eddd0fa63f501e9b368eed6ab0cc0d2e5e6da1baa570ed9e857188bae5c59c9ac89bec3fa00c8e1b725789e1af15f7b256ae7f169edfe7f3ef8834bbc8a15dd5949eb1203b1d15ae701fe4b04707a1ea54c10fef16308bf806f2144d3e7aa63705924da607162f59bff757490469c2c8d4e62a1aa0bf27323bcdaa0b17f8673fe785f6b9ff0718e55b621c8e9d92839759a98b88bd6590a0ff85e072b65d258bbfdc653444b08714c2be395b7e645caa214567b22916ffb7ebeb012d013001310130013101300130")

  val invalidSignedDigitDecompAttestation: OracleAttestmentV0TLV =
    validSignedDigitDecompAttestation.copy(sigs =
      validSignedDigitDecompAttestation.sigs.map(
        _.copy(sig = FieldElement.one)))

  val invalidSignedDigitDecompAttestation1: OracleAttestmentV0TLV =
    OracleAttestmentV0TLV(
      "fdd868fd01f2017ad485e5557110ec2a4909438ce55f02d9ec349b33989c6344fbbd19aec4ae56d800073da5fcd784525041016e96b7021bd626eed2b88fadb36d073613ab16898b34cc70a20df7bb4324db5f1287c3c1b4e3a860d578bc7327b4b1cc7573798cf071b26c8d3611a45d93c511ea6c94b548d736e79bc485d29c84f071d36c8582e9d70d86d46379556b52b280c35821425dbcefd9f68044bd36e795dc4bd875e1e28ec2d24d6de99140c670b47976d6b97f4c524cfdedd64ef95bef83c4eae85adca9a3dec4632b918aba93702a7efa5847ae64dda9dcea35c54cf42c4b8bf5cdbe5e5561eb3b37e209d762a15e2e967cea608005019e217c6b8178517df319ee82f77688fe174f4424a324e6ee4d9444343350de7454e1cb761436e60fa31bc6f0b1f98a5810dcf8f1ac393df004384c25e15834215f87be3d96c2cc2dc3e8b396f0ae24e6979e7242628852b32e94892812ab63c0c9abb2c72195161e42894f80c4d5bcf9e9c2153857dd15c9b338dc168b04e018b6c983ad41365b6e7827eb8aa2f4e89e535045a904ac19ed08c3cd49a08ad3ca98d264846223e52506c447eb12d420475c9f297957e2467eff0873ff97d8aa5fef0b363665ff6979d36d1a04eaac4d3a4f85eac1111ee9e323ae5638b019d15282cd9b18c35aa0a1b7951957ef3f012b013001300130013001300130")

  it should "validate valid enum attestations" in {
    assert(
      OracleEvent
        .verifyAttestations(enumAnnouncement,
                            validEnumAttestation,
                            signingVersion))
  }

  it should "not validate invalid enum attestations" in {
    assert(
      !OracleEvent
        .verifyAttestations(enumAnnouncement,
                            invalidEnumAttestation,
                            signingVersion))
    assert(
      !OracleEvent.verifyAttestations(enumAnnouncement,
                                      validUnsignedDigitDecompAttestation,
                                      signingVersion))
  }

  it should "validate valid unsigned digit decomp attestations" in {
    assert(
      OracleEvent.verifyAttestations(unsignedDigitDecompAnnouncement,
                                     validUnsignedDigitDecompAttestation,
                                     signingVersion))
  }

  it should "not validate invalid unsigned digit decomp attestations" in {
    assert(
      !OracleEvent.verifyAttestations(unsignedDigitDecompAnnouncement,
                                      validEnumAttestation,
                                      signingVersion))
    assert(
      !OracleEvent.verifyAttestations(unsignedDigitDecompAnnouncement,
                                      invalidUnsignedDigitDecompAttestation,
                                      signingVersion))
    assert(
      !OracleEvent.verifyAttestations(unsignedDigitDecompAnnouncement,
                                      invalidUnsignedDigitDecompAttestation1,
                                      signingVersion))
  }

  it should "validate valid signed digit decomp attestations" in {
    assert(
      OracleEvent.verifyAttestations(signedDigitDecompAnnouncement,
                                     validSignedDigitDecompAttestation,
                                     signingVersion))
  }

  it should "not validate invalid signed digit decomp attestations" in {
    assert(
      !OracleEvent.verifyAttestations(signedDigitDecompAnnouncement,
                                      validEnumAttestation,
                                      signingVersion))
    assert(
      !OracleEvent.verifyAttestations(signedDigitDecompAnnouncement,
                                      invalidSignedDigitDecompAttestation,
                                      signingVersion))
    assert(
      !OracleEvent.verifyAttestations(signedDigitDecompAnnouncement,
                                      invalidSignedDigitDecompAttestation1,
                                      signingVersion))
  }
}
