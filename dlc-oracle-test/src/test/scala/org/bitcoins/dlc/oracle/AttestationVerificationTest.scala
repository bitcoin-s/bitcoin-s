package org.bitcoins.dlc.oracle

import org.bitcoins.core.protocol.dlc.SigningVersion
import org.bitcoins.core.protocol.tlv.OracleAnnouncementV0TLV
import org.bitcoins.crypto.{FieldElement, SchnorrDigitalSignature}
import org.bitcoins.testkit.util.BitcoinSUnitTest

class AttestationVerificationTest extends BitcoinSUnitTest {

  behavior of "AttestationVerification"

  val signingVersion = SigningVersion.BasicSHA256SigningVersion

  val enumTlv = OracleAnnouncementV0TLV.fromHex(
    """fdd824a350a5f6214e14574bc925a9d5d211961a8e4e9ed8d3cb8dbc7c65d8da767152759e7d8d7646ded432717c8a4616a8fac714
      |41c47a0502522d4a58223db7798f4ac6fe4a05109781b294f778145dcde93a4e4c4408ec1ae3448c8d6da1ccc67a06fdd8223f0001
      |615050e1776b098448ee0fb38a2f94b5ee1cd41c3ca3fd5e76b5f2edf20d753e5ff64f00fdd806100003036f6e650374776f057468
      |72656504656e756d""".stripMargin)

  val validEnumAttestation = Vector(
    SchnorrDigitalSignature.fromHex(
      """615050e1776b098448ee0fb38a2f94b5ee1cd41c3ca3fd5e76b5f2edf20d753e438817863dbb363f214bbcd56297c3f576fc5475cf
        |ba924230b784826855130f""".stripMargin))

  val invalidEenumAttestation = Vector(
    SchnorrDigitalSignature.fromHex(
      """615050e1776b098448ee0fb38a2f94b5ee1cd41c3ca3fd5e76b5f2edf20d753e438817863dbb363f214bbcd56297c3f576fc5475cf
        |ba924230b784826855130e""".stripMargin))

  val unsignedDigitDecompTlv = OracleAnnouncementV0TLV.fromHex(
    """fdd824fd02aa1baefee88574d4c88f66761d8b00b9e03d035b6fa760fce4e6b5a9fb8cd3a7eef2c68f26d0a2f3f8f175b2be2839d4
      |b342b23d2bc252ee65e88a48ddb489b1e6c6fe4a05109781b294f778145dcde93a4e4c4408ec1ae3448c8d6da1ccc67a06fdd822fd
      |0244001141b1f54bd9c9eefe6d714127ee3e27ac9014813c4fd5e6b42c879f050a84bffedf6bbd50110a5fed25521d5a94de3e52cd
      |dfb23516bad07251414ec920279f047e87c0aa49d8876c3bb81f04ec531508feb601c9df7c20db0120c0cf635166d7f937f5673259
      |2175acfe50224f7172bd562f43f3007a3379d4c16c0da0ba1d8c1bb94c4e552ac57f8f8fc9a5d49da04b1ac232c8b6fa8db32cd043
      |b8397aba8bcae9c67cb51586a0d099dde46e1b1d800847f17b2e8c1cdcb45d58ba6bbe6e2bb658113f683290ce62096071777bc9fb
      |d14439079c13d84c94b7b873561f449b48af62f0a73f5ca0a9fd69f81bba47d926fb13dbae74120303bec4651098e7837726c9cce8
      |c8daf342e1db3e52133461a7e40435752b44c0e0ce3a0ee46dde0feb09ab0416706a770531d53b57200a6c93777e315926a723867c
      |8426f9dca6ddd7177a7d5011b7cc2e9320c62e9bcd91d3f9dce1c4a433ab51a89ec33357984fc1e9cee9160782cbe43480c60bc211
      |e2e02b5e38b29873988e2389bf4d8e22414c7de44173c4eb5d56ef06927e03c77b23d37fbf55ebbab894e2f720d4f93768863ad0d0
      |63ff40741e2ce49360337bf5baec30dd5774430ef9b55afa163d704fc8c19b12815275706f67ef8f8360a95ad92c2b7ee9b47c8899
      |ce5d5ec194e5908ea8707a48f7415bd853c59c21b40e056af394d5a69f46ff46e25dcebf3dc81106262772b8112bfd896fd8a44b01
      |907d7e4b176f2c7cf5d8f679e87abbcc5c105ff8f200fdd80a0f0002000
      |5756e6974730000000000110a74657374206576656e74""".stripMargin)

  val validUnsignedDigitDecompAttestation =
    """41b1f54bd9c9eefe6d714127ee3e27ac9014813c4fd5e6b42c879f050a84bffe4c4d38ea8a8b5b0bdfda933de1cd7da2390c613773
      |b477932b1817e1995647fa, df6bbd50110a5fed25521d5a94de3e52cddfb23516bad07251414ec920279f044406f66fbbc6d39f56
      |59103dc26866c90daa0620f7afe750fa471757c4eb0756, 7e87c0aa49d8876c3bb81f04ec531508feb601c9df7c20db0120c0cf63
      |5166d7351f17ceba6521af0e3fd2b2be638fe71aa9859673c56534803586aeda8812a4, f937f56732592175acfe50224f7172bd56
      |2f43f3007a3379d4c16c0da0ba1d8cbcca50d88a621ce033ed04cba19ed8febe041d7c3e8b5c2c043bfd3a1f44fae9, 1bb94c4e55
      |2ac57f8f8fc9a5d49da04b1ac232c8b6fa8db32cd043b8397aba8ba5ae4167ba045caeff80be94d846fcabe92ecc5f62fa839c644e
      |156350832f37, cae9c67cb51586a0d099dde46e1b1d800847f17b2e8c1cdcb45d58ba6bbe6e2b0015b44c4f5b85f75d302da2db52
      |113c17be9209d427708d5b124ecdf4fc0914, b658113f683290ce62096071777bc9fbd14439079c13d84c94b7b873561f449b4b7b
      |8b94cb6d4838c866efc6e6423bbec2da9256732e8110dbc4b066ec036dc9, 48af62f0a73f5ca0a9fd69f81bba47d926fb13dbae74
      |120303bec4651098e783bf6cb0731d11ab50fc447968a60ca31f8384b62400732d1a300a8c70c4700d6c, 7726c9cce8c8daf342e1
      |db3e52133461a7e40435752b44c0e0ce3a0ee46dde0f855bafb08d0f0192511723744a5e52b0280566ac2e81449f9792e50078b1cc
      |57, eb09ab0416706a770531d53b57200a6c93777e315926a723867c8426f9dca6dda4dbc159b4e6263fb47d0e67fe1e8cf2523d47
      |eca8d7f8161d3c13f62b2aec2b, d7177a7d5011b7cc2e9320c62e9bcd91d3f9dce1c4a433ab51a89ec33357984f16de4adf74a550
      |5a0db7e7c52d1e8e27f8ac26dea671c21c6cf0101c3e729431, c1e9cee9160782cbe43480c60bc211e2e02b5e38b29873988e2389
      |bf4d8e224177d3aa4c397459cb40291e3f32a7f2f4f58b2309113e11a551009d25bcc89fda, 4c7de44173c4eb5d56ef06927e03c7
      |7b23d37fbf55ebbab894e2f720d4f937688ff7755ad97a24ecf314b85ba808b67fa73f747ce863646fc2942b0e71828145, 863ad0
      |d063ff40741e2ce49360337bf5baec30dd5774430ef9b55afa163d704fe2cc732bcdf621fc75c6779e4bb450d128220af03b9ef0b0
      |bfcf19020021405f, c8c19b12815275706f67ef8f8360a95ad92c2b7ee9b47c8899ce5d5ec194e59023616a1af5ea1e788a013d68
      |e9b41f0ce04a058b629783c0ddc994af95abc604, 8ea8707a48f7415bd853c59c21b40e056af394d5a69f46ff46e25dcebf3dc811
      |c7cefa3c3a1b287f928c6c6025f7b4826163701ea1eb62006d81a97fff88898d, 06262772b8112bfd896fd8a44b01907d7e4b176f
      |2c7cf5d8f679e87abbcc5c10e00cd69683067534e2c8793d52d9edf89c8b246447387325742ba24bf802aa20""".stripMargin
      .split(",")
      .toVector
      .map(hex => SchnorrDigitalSignature.fromHex(hex.trim()))

  // this one was generated with the same public key
  val invalidUnsignedDigitDecompAttestation =
    """e54e3f52fa1d742ba15a3566fe584c67381296b30badc642970e220c55f9ed132e155a27a516c0138f676f37d1bd6ae1ab94c901df
      |79cc6890c93947cf532b13, fb867997ad4f7b8278856dcee7c9eb39819f04a2d8a51d4873eb4f2334653a02abdd9bd16f9c58a19c
      |6ddacbfb5410860438bd8509b32510312c75f935fd2be8, b68724b01e8bb618eb65c504f79f2fa8af493c7e198f5b8fb929c2e693
      |363751fdc1a6e8afdc7fb0aac7aeeb2151a795289869e6d46ae93ce1e5fb5826b9e5ec, 07e7dc59a57c3a7f1af60f41a18d039f23
      |9984acd31e179ec8d142f3080468ba9f25ab39f99153a71da2e73b3d7c5fd21b615b113960df7cad7716e94fc66f34, 1f9ee708a5
      |8961ebbbda50b80a71689c131a2c8ef5830ea81ed99b740e4e3e98fd3631b60ad70c18369fd8948870ffa2ac84ea90946367004694
      |a397ae90c3b6, 07c51bd48e0f8c316703504555eefd6b88be67a324546cbeb3fa78369d0aa5bbc5c51be90e80d8cef5770c09715e
      |c410c2d784628a8e25172acde4899bfd2f19, 1770de935e1f258d229c99cc37389ec8e4a3b87abb7df180933d2ee9c4d37815915a
      |fdd748da65e50503aeaf4c1c5fba18436d12c7a3087d098212af219fb3af, 7d2a6a7a3024c0adb7a75dc85af1e21f02f75188ea15
      |00d0560076a6256bcdb8146ad9255aad51728658498c613fd6b40a960e64743c5d61e8327f9111062e26, 554e144a91097c12f90b
      |22af639213ecf521e6e015e03758a38ebc025bb97499e94445faf628352c1cfa5eda08e4410aa19aa09743ada6ab0c38a9de99161d
      |c4, 1c618f54b4ee5e871b8353115947284b5c42330369cfc658faa62315d32fa8f0e9229d1c46018f4727b8dbb8080b2d7f351d09
      |8e33b4c999af776d1ce4021092, 8f653f89f76641c01e8df5522354dc294d4d5390a6dc57ce64de40936072e60a838d36628cf997
      |6e602bfb900dbb18f23fe9c5315b81945c623902f07db1ff60, 76c6e93fc5a2bf0aad0e1ce1e0729d145232a4cee2fa2299d0dd71
      |70a62b3191bbff460cc16d6f558b2f75e65292f41ff9ccc1fac74abe4fbc091b3a7f0c38fb, 8073cc26b81d8ad8e5f593747a6610
      |db4713527b9edeeba64b3a8952a833123fd5f7695c6cfef167dde886526de709eea44f7af3557be1e9b3aaf934b0d13251, 0564b7
      |7cc8fb2b1db50777ec17233bf76d1ada3f66718baa7ab898bce17bbb7d3a859eaa28859615874457fdb5cb022630ccecff7e2c47c2
      |6223a32c2459b137, 6826c8a2390cdf8a4781afadae6975635075727fec2f0bcccf3af7a7e3f04e55c46c10a07e5dcbbe99f1ea76
      |d74dd61ac6991d57d8bd9d7f3891b959bab5709f, 6bbe392148150ac5a5a8bd271d8ec817a7dd073f2d820846f8e8ced1f962679e
      |927d9c68d73945cb4fdfb82ca0df4d99209efd167dfe9621b83f8c7a2d530919, e50c3cc540aa30da9d87688f58cf943944e4e0df
      |8ed7c6c254af3c4b7184bfb629ab4541b3c4bf12abb2743094ca6f8c3d480fb03a4df444e8a3977434cc7e2e""".stripMargin
      .split(",")
      .toVector
      .map(hex => SchnorrDigitalSignature.fromHex(hex.trim()))

  // this one was generated with a different public key
  val invalidUnsignedDigitDecompAttestation1 =
    """894331a664253ebc1e7be8ff912cb0c067cc2b3294a44564e25815e5e83eaa063f43feb2a77a4d92a0607eb2160ea4a4da13fc37a6
      |5e4da20e66f2c027079e5c, ea806c44affb5dbad9515b615c685170d6f34558eccfaf1b6405456a3a8a626f0bf53a04a3a53caaac
      |746b7949bdc06d84edf3272412a5b84c8fc2b6c5e8c1ce, 7c173287511f3bb084755113b67ce14fc986ac6bf810702894764a82dc
      |d88ab114459798ff6a44dc9f77573e01af929a2a598ab0d4b6db3da3378180f18cd6a0, 59b70073c99dced611b3d44c5cb5e02276
      |24eeac582d6918b6846f44c739a2571dad40001b7ebb458f392a341a0a07910e77127c89f57f19f872573a097c7397, 2a1423143c
      |b019073578905aaec5277cc2226cf5fcdde1bb3e9f72cf18aab5b89f2364fc718610dc4fcad3a3e481010ccb2a7d1a7a0761a1be08
      |16561ae670ed, 5cb3e3cba05abd3bdcfd839177717c0874d5916b2080a34987c2fe63d26a112abbe4147305b621ca34bba6931789
      |9bb914ae0fe23b3e4b0d87a197b7107e1ccb, b914f3fc4b7830f77589099afb3cb84e83d41e742854e7f1bfe8d2ac943c51c9dc2a
      |12e737edf0d7aa47616cb0a534a99a6ed21d12dd58851de05ce0f886af3a, e5b50618ace293f4e7b1233354a312d90945d9c2b931
      |61f0b3a1a3bb3ef187130150bad6405a4f8ff807e4a7a2637780b4f2dc48b2d436b738db1a7c096bf533, fe0843af43da467238c7
      |8074cb12c07b3f718e5ff00f61babd596e27242edaa1ed441725a2f62d7b7d8ccea5603eba3186f99fa5fe6e78c81f15fb3a3c2fe4
      |d9, 4541d83e2e6014bfd69167fbd9ee73b9b0ad9c3fef9ca3bf793b5c73c868c5fc02b06feff46075a177c608be8cfad08d9495a1
      |89a5c3ff155a376019f74fad86, 3d612714e768794de45b8429d30643aad9900d7de91e4adf9c9970be05a284959721ad6ae0e347
      |2cfe2ad54dff7d3f697a032ccd5349a57de3497e34ff06e4c3, bbda06a310d35f3f5b8c63b8b4d5186f737bff86a38a3ec2f4b8e4
      |9892754f346a91bf60423b57cf2591742776f25e3c172d2722c1a7161cdd3c8d68604ce039, a61b20b235de73009520ae823a63ec
      |fd46a901619d9a977a5a18bbb11986c0063441f8d45db7b90f649e3f1da92e1a6ba4b95256e64bf48ac2c4cac6248acf19, 2d2292
      |cb193da1a7dcc1e959818778be2a0aa7f0ce21a8e0472cd6aa9a72092f33fefbd24f7a94c73dd05fab5f124ad83aee28287bc36672
      |ed3cbce9c9b6add4, 2738f8f9b7f333a527ced0dd95fc0f32fb61b4bb908b15e806ca859e204cad6bf58c6e69750b89db038de056
      |6c941bbf674fc1446c286f46f143c168e9b87487, 4e6bdfc3d072e9969f1a9b9c7efbad85f7c794b75bf3ec5177b6aa09fca3ed64
      |3bb9a9a136c001c401fae7428a3491c32056cf58a14ea53d6c02dda160541bbb, 46a69c3a16540910f8af70fa15f5e47189053429
      |1cd2268041c14026e17c8d406f0fe1ad62b7ca4ff406e5bb2b17b76a16bc7c6223d7f70e03da8ce5f02c7b54""".stripMargin
      .split(",")
      .toVector
      .map(hex => SchnorrDigitalSignature.fromHex(hex.trim()))

  val signedDigitDecompTlv = OracleAnnouncementV0TLV.fromHex(
    """fdd824fd02cdf404acc6db19bf5e81920703f766b427a0e804c14c706671a91e6502db81d84318aa4c7f62df8cd67df2d82810d68b
      |66b56f9565bec5ebfe462246f737fd33a6a2494b98b5383e235a83fcdb1242c7d2d169329c106e3bf7dba288cf1f91b796fdd822fd
      |02670012d1fe79256eb0a83ce549ba19b9cbf3eb31ece55297c0552855ae8871a43b91d80898c0b45a5b5e496770d6f7cc498a77b5
      |54727b24ff3af348dc21313a4e88be11cc8a5d6bd88f582bb3d8044ae55818d0194c72867ac5a5c1bbef42f75c3480411e2ab0fa3c
      |ecc65d0eda07a815ce1ebfbe1eddb40da5fd474f17afc942098e469dd8d6d7f0ca7bee65577e049af4e7a9387dc8bf07d8d119aa7f
      |76e3c6cbf713dbcbd5474325eb6e1ed7a7489bafca3cfc0011d05312b6a2ed1a5fdf3052987e9a6bfdb2d13b65cfa7494888b72a8f
      |d255a9c07626b25347c2348b0b3cc0ca34216e2d26ae72d9b21a5b2c1244db186b66bb58dc98a990e3308f4cf1af1b5462143d901a
      |aa6be17d7445b1c34945a4db6e6e3761d5f3b3b516c4a4841fd4419127b77a2090c2a803b79baa6becdd28bfce031b5eb4d00cdfe8
      |3b0ba77d21c62198df9aeebe0e3c528834684504196a1d4711ad84a50aef0d1d9651dd2d0318a0cfa02c934d9691a1c6f6bfd6cb39
      |bf080b5cbf027f8507cce0bdbf85888538f4bebbf2941227d5550dd9119cde6f2eb7b2d9e83ae35d0b718060d3636034baff738866
      |833a4236bb9208628f9f20a90be5dd6c4e87434ba30d774e7a085f8178300891b8048767ee493cc12bdf4435951f546fd179186055
      |71f94341d0990a6f1b5224e59ead4629084a831044d3a16c5c6f0bb42ceb776be4e493da2d84345090359b7874920f2d73afa952b4
      |59b410ec207c2c1ab7d07c983f4b0353cceb43d6921348c93f65f3db71c6bb5e786d49fb087396db299003d29aaa0ef7394d5ff64f
      |00fdd80a11000201074254432f5553440000000000110b7369676e65642074657374""".stripMargin)

  val validSignedDigitDecompAttestation =
    """d1fe79256eb0a83ce549ba19b9cbf3eb31ece55297c0552855ae8871a43b91d890e65b9f6c6d5f438d631b96d4d54ad5d73f1070f5
      |50669430908bae91d247b4, 0898c0b45a5b5e496770d6f7cc498a77b554727b24ff3af348dc21313a4e88be9cc5f16b91dcd20a44
      |11c34013dc090947d554a946e325c96d64b3e0bfa02c4b, 11cc8a5d6bd88f582bb3d8044ae55818d0194c72867ac5a5c1bbef42f7
      |5c3480ce29b3e56b7adfd4e08ae5559456e5f838e8828ac6d882de2343dbdb78cbd946, 411e2ab0fa3cecc65d0eda07a815ce1ebf
      |be1eddb40da5fd474f17afc942098eb65c911cb42f4bb5c45477745c6231bf0dc1915223eb7a4e7754dddc4d9ddf28, 469dd8d6d7
      |f0ca7bee65577e049af4e7a9387dc8bf07d8d119aa7f76e3c6cbf7b16742e6afd4edddc8ffb2dce5182484a99c09ed39fc3d60235c
      |0aa5bd78f452, 13dbcbd5474325eb6e1ed7a7489bafca3cfc0011d05312b6a2ed1a5fdf3052983bfdd5d439f6896710da9472e131
      |8c79dcc071696b29e2a92bbe6112e8728604, 7e9a6bfdb2d13b65cfa7494888b72a8fd255a9c07626b25347c2348b0b3cc0cac7bb
      |47f019b655169c05f1ed0df70626186d8ecb29b3bc9345055cd424f0a656, 34216e2d26ae72d9b21a5b2c1244db186b66bb58dc98
      |a990e3308f4cf1af1b5452458df891c29a41088fc8099c30a82db9605a4ac02b9b2bce59705d07e95b65, 62143d901aaa6be17d74
      |45b1c34945a4db6e6e3761d5f3b3b516c4a4841fd4413eb8a0915929d090eb3216976d87663f4d1c9a6c514c8ba504e5b1d0dd0368
      |3c, 9127b77a2090c2a803b79baa6becdd28bfce031b5eb4d00cdfe83b0ba77d21c6c8d686e1293bcef7e28cd2a52df5ab6df2657c
      |e47dea37be6f7564924f26d0d7, 2198df9aeebe0e3c528834684504196a1d4711ad84a50aef0d1d9651dd2d03183899798e840814
      |f1b1b7a06741f9ae0d711da6a139985a3c0e001c0f49c49ad0, a0cfa02c934d9691a1c6f6bfd6cb39bf080b5cbf027f8507cce0bd
      |bf85888538e3043866b40f620d475cb0134848eee7952466b21aa50c814211894d6fc6936b, f4bebbf2941227d5550dd9119cde6f
      |2eb7b2d9e83ae35d0b718060d3636034ba287326fa40e7b2e8762d34aa6045f02af0af23de35312cf0b458d5989aca062f, ff7388
      |66833a4236bb9208628f9f20a90be5dd6c4e87434ba30d774e7a085f810a541193af75adc095ab7bb1f105257402ab54a9c3670a37
      |aac8f2dc7e16e393, 78300891b8048767ee493cc12bdf4435951f546fd17918605571f94341d0990adff225073e691b86cca91ddc
      |7bd7940391c46b520c6812129c316800ec6a5315, 6f1b5224e59ead4629084a831044d3a16c5c6f0bb42ceb776be4e493da2d8434
      |9509950d78c834cfc21aa400be2d2ae5c061835a5ac3c5a30fc0568b98f24c88, 5090359b7874920f2d73afa952b459b410ec207c
      |2c1ab7d07c983f4b0353cceb2cd65510ffb0a618728d41483c4530352deab665ad9cd18611a727dfe81c332b, 43d6921348c93f65
      |f3db71c6bb5e786d49fb087396db299003d29aaa0ef7394d51ce6da143a6b4ae6f8cc62ee4edb825d84c882dfbfee8a67baf283791
      |4171dd""".stripMargin
      .split(",")
      .toVector
      .map(hex => SchnorrDigitalSignature.fromHex(hex.trim()))

  val invalidSignedDigitDecompAttestation =
    """efd860f8c83bf397dab58c7894cd584851ad2a5e62b7b18a1abd6739f4b2d6e3149dd0f53a5200f62465577f2cbb158f9c459fa57b
      |ddcebb67ffb5547b0aa042, 68375ae87668e8f99ba2f43c0608277b037f392a9cc3cc30c284b1845a3e058d8e8a2399fa7da6b564
      |fddd4aeca31ebdce0c7e8888764f979ffa5d64178550ec, e68b02e8e8a35a358764a1b272dd6ef19b139a5c16be15653b6c178020
      |2ffe0ba84742d3e9354e8b04c1c63a7c2efe2b3a98c2ba152bdf978525236e2953a744, 4162aad256061a6cf40adbcaa7d0699313
      |0020570ead6b168f0713510ea7a83fef4a1a720a4c505fcab93af9a55622e242963291a9bbe3d8b31b641e25ef62b8, 2c625dd227
      |bc9718e441d55c2518b72b60e29a7707409822a2faaf90619ee8fd816560c83439948b650aa17cb3930314365bc7dec3454f7bb51e
      |1eddc9306235, 5361bc7f5c5760848cc9b4f7bc8ff6dbaef72468a6941f4aba1b2656fc43f69246f3a7ae46c016bd416a1a625a1a
      |7c7bce0c5d30c78410cc816323159ffb901a, a416336ab563cc65f238e6d9b177e740c2e409d57274c6769d96331ef63b3a985a99
      |cb6b10618e61465d1f5867cb22b8927e5c1d4a6ff9639b9290de5c4976a8, 5423e9e82de856bc01433061e0c03161880bd2e79d34
      |f38005f9d0a987f913cfd619cdd3ea2762669ef24eb43fa123e1d122326fd8d74bffd5dfb1efc6b581a0, 10065107a76af48168f3
      |e906e064b769abf58f2f3f36dd516c4e8375759d8ad071cbbe012537f83533363d8f02dfd21a637adbacab788d3f231a44fa4fdafb
      |4e, 9fadcde44319c3983842a034dea5c30b75a2fbf133511ba5bea0ee94f02255ac0393c6307b66ac15ccb3559337cbf7228e635c
      |4ecef66560ecc286c7ddb82d6d, d71f08cc3daffaa912906eab5802cf80012d898b1f65c65b95e7444626d2d8c29d94940309f6d2
      |3697e01fbd45994d45c5569ba0188bef96e3a4fd3741a40cd7, e8e2ebbdd5057bf4a73c0c24b7b890ff3fb658291cc2f509d8f365
      |689b21409e88b02a40ee906f088f330437cf6a64c1216021f4368e1efa28a3063ed3cfe4dd, 9b9da54e538318baf6e0c38bc30740
      |b132ef5dfba869ea8e9202886485fa4466379d075d0ba0448eada877cd6551dabd93e687db835562728f4d168a34b3c763, 35c100
      |98ebf6e9275c53b9627143ff725fc1f3fee099db92ca6b6c7fc6011dcc71e977b5a248d0fa411d2eef89862913df9715b46072f4ce
      |78d9046fe4f9fe40, 023dd770b1b50bdecd3809228c38425b576abe7a317c661bafa70886e1c81d17a115ff41d8350077dc704af2
      |3e9bcdb5c9fa951d458b9ca596e75f7f9286e4e6, 0c3b88a84dec9f8f7d46a76da58f45cb3afe2dadb53784ca3be1dde096723f9f
      |813b8d8e23f1b3d4de36c44ffa671d27e4c7d76b1d28b192214cbeef5cf23ba9, a8c3b01bf9a58e614b0c91549263bc85f5c94c0b
      |5a7d56fd09950515ec01339f41804f10e1f82a9821ef979a5d53df89cb81dc3f111ed1fc189715ea0ac7143c, 221156f56f86adb9
      |b2a0290e9e5884c0aa150caf55e3697fe36115ad2761e9530315258258ec7a53321d306a98a2a040873ceca310bd24aee311f54335
      |b99e95""".stripMargin
      .split(",")
      .toVector
      .map(hex => SchnorrDigitalSignature.fromHex(hex.trim()))

  val invalidSignedDigitDecompAttestation1 =
    """d3cde93454a71fa1cac84130ce240789d5bc344dfd825b14872d791abd8e7a78c1c0fa2b6f3bdf18bc004bcf2ef5267d77a640340c
      |a6a05ed3ee5ec09bd2830b, cd2931126ea4fa1dce02b3e9e739eb417636f21a86862dc5ef19d9dc6ddabb8c3c2fc7bbda31108b83
      |a36fcbfa830a41260377b09a5f055bc4796e0f1f5dac6d, 68bb7e1a306c30d90418dc2912f79c32a3f52d8946c7909f7054386de9
      |2135210a8e994bf293b17b33e211d5b1df8f6d396184035216d9600098ef6c774856a7, cff3e46c70c930f9671ad295e610228d22
      |017f09d75168527005f5c84ee099593d95a05c8ec23437df1ace69e74ac370db9cfd41dae98ad8610ec5a068f77292, 259b17e5e6
      |501f2bc792a24cd14a079211b9b66f040fcd20312a0abf2fe0579d6081d156d373d1af9ff99ec7aab5bfe3eb92e0d57f862da0035a
      |a53c9c693d3e, 513de51ee3d995800aaf0fdffb48c45f86bcfe66eef5f0cf28fe4522b52a3927e7236973b7dd7848f155fbcc57df
      |771f49863e4d7bed6b7f8f4bfffdcf01f77f, ab32b6db4eb0bc712ee509aa187c097c061a25440827de631dbe42ba14c9cd4571a4
      |995774455246e1a528d3e024e8ff777ceaadacdc6485189de4aac7d5fa2a, a6f24a9bfc431dfd2f732aa642e158b3737a95020c8d
      |b14fc7fcc8f4ff9ebac335c5220ef5a90b3a42695b6093dc291549fb7de76b320c1b88d06d0d1cc7c5a0, 73c4ef0ecb004de78359
      |8b65fd634e8b586815b1930a7bb4899f6abcda3ccd26e93fcf610614d488e86d30c1ceac4c9a2a16b8f09585b77786d6858e10473c
      |39, f0bb14c407ab1f87fc7a3c0ce66595e3ea5500040819dda4abc7e73ab482fbe4e058a84ff6d7185aa288c2c0296a25ac8baa3d
      |0a7678d2a5bfd1dfb3d9a7abc7, e40e556c8bd03818cb5a571c47ab2f3c3cd3ec520368cfee6ce16ed5edb09e1d2a92f26208f0e2
      |d06839c8b246b8c54867381f617e539e9e1d87c3418dedf5b2, e9b25c1bac31ccb6ce8058a9c8080ba8b99a8a20f116cf385dcafd
      |3fc1f78e1a1d549cb8a50cbf06b005e56a4326f5e74647d818c73c6c52f73743660ad192cf, 569443b77f45d26e68ffd1cd95813d
      |adfc34fef15476f7c2a99974d636aa52353f4fd7b19326e937fdbaefffeee061b8e29934440f5a75803fab2770bcf24090, f84505
      |a25f513733f14857973898e648ce774e658b65eed83d8dece4fc5727f7c6143aa53d2e3bb3c24df6d458e8244e1e306807d4577b8e
      |60c2764f259c7e48, 3152ca0e3e9704975f0ad9d48fd386bbfd42582bd757cf95ab2685535fe3dd689eb2a1b6703b141ecc051996
      |cb88e98920d00771a97b434fea2873814752f60a, 5571c3130d30165a456db4611ce4811455b50e515fa0270a988af88b1b102b94
      |923f9c9a6b900c4761b5a06ffe520e06e6798359101dc21b4d3ef8387a985393, 339c265ad362dd662a60711a31287a32c4573e6e
      |6777681005925aca36d0dc7d2eb2c2d8780854cd161f262aaedfced130c39d45f73f9806448f21f616450cd1, c76099ad411c73fd
      |0cb43a5bac1d467590bb2d49defe62aa65b903ee66a7aa370d9351fc4ef7ce0ee1dcb940f684dbbc81b8fd4495ea89cbbd1814284f
      |db3d6e""".stripMargin
      .split(",")
      .toVector
      .map(hex => SchnorrDigitalSignature.fromHex(hex.trim()))

  it should "validate valid enum attestations" in {
    assert(
      OracleEvent
        .verifyAttestations(enumTlv, validEnumAttestation, signingVersion))
  }

  it should "not validate invalid enum attestations" in {
    assert(
      !OracleEvent.verifyAttestations(enumTlv, Vector.empty, signingVersion))
    assert(
      !OracleEvent
        .verifyAttestations(enumTlv, invalidEenumAttestation, signingVersion))
    assert(
      !OracleEvent.verifyAttestations(
        enumTlv,
        validEnumAttestation ++ validEnumAttestation,
        signingVersion))
    assert(
      !OracleEvent.verifyAttestations(enumTlv,
                                      validUnsignedDigitDecompAttestation,
                                      signingVersion))
  }

  it should "validate valid unsigned digit decomp attestations" in {
    assert(
      OracleEvent.verifyAttestations(unsignedDigitDecompTlv,
                                     validUnsignedDigitDecompAttestation,
                                     signingVersion))
  }

  it should "not validate invalid unsigned digit decomp attestations" in {
    assert(
      !OracleEvent.verifyAttestations(unsignedDigitDecompTlv,
                                      Vector.empty,
                                      signingVersion))
    assert(
      !OracleEvent.verifyAttestations(unsignedDigitDecompTlv,
                                      validEnumAttestation,
                                      signingVersion))
    assert(
      !OracleEvent.verifyAttestations(unsignedDigitDecompTlv,
                                      invalidUnsignedDigitDecompAttestation,
                                      signingVersion))
    assert(
      !OracleEvent.verifyAttestations(unsignedDigitDecompTlv,
                                      invalidUnsignedDigitDecompAttestation1,
                                      signingVersion))
    assert(
      !OracleEvent.verifyAttestations(
        unsignedDigitDecompTlv,
        validUnsignedDigitDecompAttestation.reverse,
        signingVersion))
    assert(
      !OracleEvent.verifyAttestations(
        unsignedDigitDecompTlv,
        SchnorrDigitalSignature(
          validUnsignedDigitDecompAttestation.head.rx,
          FieldElement.fromHex(
            "0001020304050607080910111213141516171819202122232425262728293031")) +: validUnsignedDigitDecompAttestation.tail,
        signingVersion
      ))
  }

  it should "validate valid signed digit decomp attestations" in {
    assert(
      OracleEvent.verifyAttestations(signedDigitDecompTlv,
                                     validSignedDigitDecompAttestation,
                                     signingVersion))
  }

  it should "not validate invalid signed digit decomp attestations" in {
    assert(
      !OracleEvent
        .verifyAttestations(signedDigitDecompTlv, Vector.empty, signingVersion))
    assert(
      !OracleEvent.verifyAttestations(signedDigitDecompTlv,
                                      validEnumAttestation,
                                      signingVersion))
    assert(
      !OracleEvent.verifyAttestations(signedDigitDecompTlv,
                                      invalidSignedDigitDecompAttestation,
                                      signingVersion))
    assert(
      !OracleEvent.verifyAttestations(signedDigitDecompTlv,
                                      invalidSignedDigitDecompAttestation1,
                                      signingVersion))
    assert(
      !OracleEvent.verifyAttestations(signedDigitDecompTlv,
                                      validSignedDigitDecompAttestation.reverse,
                                      signingVersion))
    assert(
      !OracleEvent.verifyAttestations(
        signedDigitDecompTlv,
        SchnorrDigitalSignature(
          validSignedDigitDecompAttestation.head.rx,
          FieldElement.fromHex(
            "0001020304050607080910111213141516171819202122232425262728293031")) +: validUnsignedDigitDecompAttestation.tail,
        signingVersion
      ))
  }

}
