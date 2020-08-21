package org.bitcoins.keymanager.bip39

import java.nio.file.Files

import org.bitcoins.core.config.{MainNet, RegTest}
import org.bitcoins.core.crypto.MnemonicCode
import org.bitcoins.core.hd._
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.core.wallet.keymanagement
import org.bitcoins.core.wallet.keymanagement.{
  InitializeKeyManagerError,
  KeyManagerParams
}
import org.bitcoins.crypto.{AesPassword, DoubleSha256DigestBE}
import org.bitcoins.keymanager._
import org.bitcoins.testkit.keymanager.{
  KeyManagerApiUnitTest,
  KeyManagerTestUtil
}
import scodec.bits.BitVector

class BIP39KeyManagerApiTest extends KeyManagerApiUnitTest {
  val purpose = HDPurposes.Legacy

  //this is taken from 'trezor-addresses.json' which give us test cases that conform with trezor
  val mnemonicStr =
    "stage boring net gather radar radio arrest eye ask risk girl country"
  val mnemonic = MnemonicCode.fromWords(mnemonicStr.split(" ").toVector)

  val coin = HDCoin(purpose, coinType = HDCoinType.Bitcoin)
  val hdAccount = HDAccount(coin, 0)

  val path: HDPath =
    LegacyHDPath(coin.coinType, coin.purpose.constant, HDChainType.External, 0)

  it must "initialize the key manager" in {
    val entropy = MnemonicCode.getEntropy256Bits
    val keyManager = withInitializedKeyManager(entropy = entropy)
    val seedPath = keyManager.kmParams.seedPath
    //verify we wrote the seed
    assert(WalletStorage.seedExists(seedPath),
           "KeyManager did not write the seed to disk!")

    val decryptedE =
      WalletStorage.decryptMnemonicFromDisk(seedPath,
                                            KeyManagerTestUtil.badPassphrase)

    val mnemonic = decryptedE match {
      case Right(m) => m
      case Left(err) =>
        fail(
          s"Failed to read mnemonic that was written by key manager with err=${err}")
    }

    assert(mnemonic.mnemonicCode.toEntropy == entropy,
           s"We did not read the same entropy that we wrote!")
  }

  it must "initialize the key manager with a specific mnemonic" in {

    val kmParams = buildParams()

    val keyManager = withInitializedKeyManager(kmParams = kmParams,
                                               entropy = mnemonic.toEntropy,
                                               bip39PasswordOpt = None)

    keyManager.deriveXPub(hdAccount).get.toString must be(
      "xpub6D36zpm3tLPy3dBCpiScEpmmgsivFBcHxX5oXmPBW982BmLiEkjBEDdswxFUoeXpp272QuSpNKZ3f2TdEMkAHyCz1M7P3gFkYJJVEsM66SE")
  }

  it must "initialize a key manager to the same xpub if we call constructor directly or use CreateKeyManagerApi" in {
    val kmParams = buildParams()
    val direct = BIP39KeyManager(mnemonic, kmParams, None, TimeUtil.now)

    val directXpub = direct.getRootXPub

    val api = BIP39KeyManager
      .initializeWithEntropy(entropy = mnemonic.toEntropy,
                             bip39PasswordOpt = None,
                             kmParams = kmParams)
      .right
      .get

    val apiXpub = api.getRootXPub

    assert(apiXpub == directXpub,
           s"We don't have initialization symmetry between our constructors!")

    //we should be able to derive the same child xpub
    assert(api.deriveXPub(hdAccount) == direct.deriveXPub(hdAccount))
  }

  it must "initialize a key manager with a bip39 password to the same xpub if we call constructor directly or use CreateKeyManagerApi" in {
    val kmParams = buildParams()
    val bip39Pw = KeyManagerTestUtil.bip39Password
    val direct =
      BIP39KeyManager(mnemonic, kmParams, Some(bip39Pw), TimeUtil.now)

    val directXpub = direct.getRootXPub

    val api = BIP39KeyManager
      .initializeWithEntropy(mnemonic.toEntropy, Some(bip39Pw), kmParams)
      .right
      .get

    val apiXpub = api.getRootXPub

    assert(apiXpub == directXpub,
           s"We don't have initialization symmetry between our constructors!")

    //we should be able to derive the same child xpub
    assert(api.deriveXPub(hdAccount) == direct.deriveXPub(hdAccount))
  }

  it must "write a key manager to disk then initialize a key manager from a params" in {
    val kmParams = buildParams()
    val bip39Pw = KeyManagerTestUtil.bip39Password
    val direct =
      BIP39KeyManager(mnemonic, kmParams, Some(bip39Pw), TimeUtil.now)

    val decryptedMnemonic = DecryptedMnemonic(mnemonic, direct.creationTime)
    val password = AesPassword.fromNonEmptyString("password")
    WalletStorage.writeMnemonicToDisk(kmParams.seedPath,
                                      decryptedMnemonic.encrypt(password))

    val directXpub = direct.getRootXPub
    val api =
      BIP39KeyManager.fromParams(kmParams, password, Some(bip39Pw)).right.get

    val apiXpub = api.getRootXPub

    assert(apiXpub == directXpub,
           s"We don't have initialization symmetry between our constructors!")

    //we should be able to derive the same child xpub
    assert(api.deriveXPub(hdAccount) == direct.deriveXPub(hdAccount))
  }

  it must "initialize a key manager from a mnemonic to the same xpub if we call constructor directly or use CreateKeyManagerApi" in {
    val kmParams = buildParams()
    val bip39Pw = KeyManagerTestUtil.bip39Password
    val direct =
      BIP39KeyManager(mnemonic, kmParams, Some(bip39Pw), TimeUtil.now)

    val directXpub = direct.getRootXPub

    val api = BIP39KeyManager
      .initializeWithMnemonic(mnemonic, Some(bip39Pw), kmParams)
      .right
      .get

    val apiXpub = api.getRootXPub

    assert(apiXpub == directXpub,
           s"We don't have initialization symmetry between our constructors!")

    //we should be able to derive the same child xpub
    assert(api.deriveXPub(hdAccount) == direct.deriveXPub(hdAccount))
  }

  it must "NOT initialize a key manager to the same xpub if one has a password and one does not" in {
    val kmParams = buildParams()
    val bip39Pw = KeyManagerTestUtil.bip39PasswordNonEmpty

    val withPassword =
      BIP39KeyManager(mnemonic, kmParams, Some(bip39Pw), TimeUtil.now)
    val withPasswordXpub = withPassword.getRootXPub

    val noPassword =
      BIP39KeyManager(mnemonic, kmParams, None, TimeUtil.now)

    val noPwXpub = noPassword.getRootXPub

    assert(
      withPasswordXpub != noPwXpub,
      s"A key manager with a BIP39 passwrod should not generate the same xpub as a key manager without a password!")

  }

  it must "return a mnemonic not found if we have not initialized the key manager" in {
    val kmParams = buildParams()
    val kmE = BIP39KeyManager.fromParams(kmParams = kmParams,
                                         password =
                                           BIP39KeyManager.badPassphrase,
                                         bip39PasswordOpt = None)

    assert(kmE == Left(ReadMnemonicError.NotFoundError))
  }

  it must "sign something with the key manager" in {
    val keyManager = withInitializedKeyManager()
    val hash = DoubleSha256DigestBE.empty.bytes
    val signer = keyManager.toSign(path)
    val sig = signer.sign(hash)
    assert(signer.publicKey.verify(hash, sig))
  }

  it must "throw an exception if entropy is bad" in {
    val badEntropy = BitVector.empty

    val init = BIP39KeyManager.initializeWithEntropy(
      entropy = badEntropy,
      bip39PasswordOpt = KeyManagerTestUtil.bip39PasswordOpt,
      kmParams = buildParams())

    assert(init == Left(InitializeKeyManagerError.BadEntropy))
  }

  it must "read an existing seed from disk if we call initialize and one already exists" in {
    val seedPath = KeyManagerTestUtil.tmpSeedPath
    val kmParams =
      keymanagement.KeyManagerParams(seedPath, HDPurposes.SegWit, RegTest)
    val entropy = MnemonicCode.getEntropy256Bits
    val passwordOpt = Some(KeyManagerTestUtil.bip39Password)
    val keyManager = withInitializedKeyManager(kmParams = kmParams,
                                               entropy = entropy,
                                               bip39PasswordOpt = passwordOpt)

    assert(Files.exists(keyManager.kmParams.seedPath),
           s"Seed path must exist after calling withInitializedKeyManager")

    val firstXpub = keyManager.getRootXPub

    //now let's try to initialize again, our xpub should be exactly the same
    val keyManager2E =
      BIP39KeyManager.initialize(kmParams, bip39PasswordOpt = passwordOpt)
    keyManager2E match {
      case Left(_) =>
        fail(s"Must have been able to intiialize the key manager for test")
      case Right(km2) =>
        assert(km2.getRootXPub == firstXpub)
    }

  }

  private def buildParams(): KeyManagerParams = {
    keymanagement.KeyManagerParams(seedPath = KeyManagerTestUtil.tmpSeedPath,
                                   purpose = purpose,
                                   network = MainNet)
  }
}
