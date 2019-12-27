package org.bitcoins.keymanager.bip39

import org.bitcoins.core.config.MainNet
import org.bitcoins.core.crypto.{DoubleSha256DigestBE, MnemonicCode}
import org.bitcoins.core.hd._
import org.bitcoins.keymanager._
import scodec.bits.BitVector

class BIP39KeyManagerTest extends KeyManagerUnitTest {
  val purpose = HDPurposes.Legacy
  //this is taken from 'trezor-addresses.json' which give us test cases that conform with trezor
  val mnemonicStr ="stage boring net gather radar radio arrest eye ask risk girl country"
  val mnemonic = MnemonicCode.fromWords(mnemonicStr.split(" ").toVector)

  val coin = HDCoin(purpose,coinType = HDCoinType.Bitcoin)
  val hdAccount = HDAccount(coin, 0)
  val path: HDPath  = LegacyHDPath(coin.coinType,coin.purpose.constant,HDChainType.External,0)

  it must "initialize the key manager" in {
    val entropy = MnemonicCode.getEntropy256Bits
    val keyManager = withInitializedKeyManager(entropy = entropy)
    val seedPath = keyManager.kmParams.seedPath
    //verify we wrote the seed
    assert(WalletStorage.seedExists(seedPath), "KeyManager did not write the seed to disk!")

    val decryptedE = WalletStorage.decryptMnemonicFromDisk(seedPath, KeyManagerTestUtil.badPassphrase)

    val mnemonic = decryptedE match {
      case Right(m) => m
      case Left(err) => fail(s"Failed to read mnemonic that was written by key manager with err=${err}")
    }

    assert(mnemonic.toEntropy == entropy, s"We did not read the same entropy that we wrote!")
  }

  it must "initialize the key manager with a specific mnemonic" in {

    val kmParams = buildParams()

    val keyManager = withInitializedKeyManager(kmParams = kmParams,
      entropy = mnemonic.toEntropy)


    keyManager.deriveXPub(hdAccount).get.toString must be ("xpub6D36zpm3tLPy3dBCpiScEpmmgsivFBcHxX5oXmPBW982BmLiEkjBEDdswxFUoeXpp272QuSpNKZ3f2TdEMkAHyCz1M7P3gFkYJJVEsM66SE")
  }

  it must "initialize a key manager to the same xpub if we call constructor directly or use CreateKeyManagerApi" in {
    val kmParams = buildParams()
    val direct = BIP39KeyManager(mnemonic, kmParams)

    val directXpub = direct.getRootXPub

    val api = BIP39KeyManager.initializeWithEntropy(mnemonic.toEntropy, kmParams).right.get

    val apiXpub = api.getRootXPub

    assert(apiXpub == directXpub, s"We don't have initialization symmetry between our constructors!")


    //we should be able to derive the same child xpub
    assert(api.deriveXPub(hdAccount) == direct.deriveXPub(hdAccount))
  }

  it must "return a mnemonic not found if we have not initialized the key manager" in {
    val kmParams = buildParams()
    val kmE = BIP39KeyManager.fromParams(kmParams, BIP39KeyManager.badPassphrase)

    assert(kmE == Left(ReadMnemonicError.NotFoundError))
  }

  it must "sign something with the key manager" in {
    val keyManager = withInitializedKeyManager()
    val hash = DoubleSha256DigestBE.empty.bytes
    val signer = keyManager.toSign(path)
    val sig = signer.sign(hash)
    assert(signer.publicKey.verify(hash,sig))
  }

  it must "throw an exception if entropy is bad" in {
    val badEntropy = BitVector.empty


    val init = BIP39KeyManager.initializeWithEntropy(badEntropy, buildParams())

    assert(init == Left(InitializeKeyManagerError.BadEntropy))
  }

  private def buildParams(): KeyManagerParams = {
    KeyManagerParams(seedPath = KeyManagerTestUtil.tmpSeedPath,
      purpose = purpose, network = MainNet)
  }
}
