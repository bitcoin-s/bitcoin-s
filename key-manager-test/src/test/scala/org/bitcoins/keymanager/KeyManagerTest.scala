package org.bitcoins.keymanager

import org.bitcoins.core.config.MainNet
import org.bitcoins.core.crypto.MnemonicCode
import org.bitcoins.core.hd.{HDAccount, HDCoin, HDCoinType, HDPurposes}

class KeyManagerTest extends KeyManagerUnitTest {
  val purpose = HDPurposes.Legacy

  //this is taken from 'trezor-addresses.json' which give us test cases that conform with trezor
  val mnemonicStr ="stage boring net gather radar radio arrest eye ask risk girl country"
  val mnemonic = MnemonicCode.fromWords(mnemonicStr.split(" ").toVector)

  val coin = HDCoin(purpose,coinType = HDCoinType.Bitcoin)
  val hdAccount = HDAccount(coin, 0)

  it must "initialize the key manager" in {
    val entropy = MnemonicCode.getEntropy256Bits
    val keyManager = withInitializedKeyManager(entropy = entropy)
    val seedPath = keyManager.kmParams.seedPath
    //verify we wrote the seed
    assert(WalletStorage.seedExists(seedPath), "KeyManager did not write the seed to disk!")

    val decryptedR: ReadMnemonicResult = WalletStorage.decryptMnemonicFromDisk(seedPath, KeyManagerTestUtil.badPassphrase)

    val mnemonic = decryptedR match {
      case ReadMnemonicSuccess(m) => m
      case err: ReadMnemonicError => fail(s"Failed to read mnemonic that was written by key manager with err=${err}")
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
    val direct = KeyManager(mnemonic, kmParams)

    val directXpub = direct.getXPub

    val api = KeyManager.initializeWithEntropy(mnemonic.toEntropy, kmParams)
      .asInstanceOf[InitializeKeyManagerSuccess].keyManager

    val apiXpub = api.getXPub

    assert(apiXpub == directXpub, s"We don't have initialization symmetry between our constructors!")


    //we should be able to derive the same child xpub
    assert(api.deriveXPub(hdAccount) == direct.deriveXPub(hdAccount))
  }

  it must "return a mnemonic not found if we have not initialized the key manager" in {
    val kmParams = buildParams()
    val kmE = KeyManager.fromParams(kmParams, KeyManager.badPassphrase)

    assert(kmE == Left(ReadMnemonicError.NotFoundError))
  }

  private def buildParams(): KeyManagerParams = {
    KeyManagerParams(seedPath = KeyManagerTestUtil.seedPath,
      purpose = purpose, network = MainNet)
  }
}
