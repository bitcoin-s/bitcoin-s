package org.bitcoins.keymanager.bip39

import org.bitcoins.core.api.keymanager.KeyManagerApi
import org.bitcoins.core.config.{MainNet, RegTest}
import org.bitcoins.core.crypto.{BIP39Seed, MnemonicCode}
import org.bitcoins.core.hd._
import org.bitcoins.core.util.{HDUtil, TimeUtil}
import org.bitcoins.core.wallet.keymanagement
import org.bitcoins.core.wallet.keymanagement.KeyManagerUnlockError.JsonParsingError
import org.bitcoins.core.wallet.keymanagement.{
  InitializeKeyManagerError,
  KeyManagerParams,
  KeyManagerUnlockError
}
import org.bitcoins.crypto.{AesPassword, DoubleSha256DigestBE}
import org.bitcoins.keymanager._
import org.bitcoins.testkit.keymanager.{
  KeyManagerApiUnitTest,
  KeyManagerTestUtil
}
import scodec.bits.BitVector

import java.nio.file.Files

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

  it must "compare key managers correctly" in {
    val km = withInitializedKeyManager()
    val dummy = new KeyManagerApi {}

    assert(km == km)
    assert(km != dummy)
  }

  it must "initialize the key manager" in {
    val entropy = MnemonicCode.getEntropy256Bits
    val aesPasswordOpt = KeyManagerTestUtil.aesPasswordOpt
    val keyManager =
      withInitializedKeyManager(aesPasswordOpt = aesPasswordOpt,
                                entropy = entropy)
    val seedPath = keyManager.kmParams.seedPath
    //verify we wrote the seed
    assert(WalletStorage.seedExists(seedPath),
           "KeyManager did not write the seed to disk!")

    val decryptedE =
      WalletStorage.decryptSeedFromDisk(seedPath, aesPasswordOpt)

    decryptedE match {
      case Right(mnemonic: DecryptedMnemonic) =>
        assert(mnemonic.mnemonicCode.toEntropy == entropy,
               s"We did not read the same entropy that we wrote!")
      case Right(xprv: DecryptedExtPrivKey) =>
        fail(s"Parsed unexpected type of seed $xprv")
      case Left(err) =>
        fail(
          s"Failed to read mnemonic that was written by key manager with err=$err")
    }
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
    val direct =
      BIP39KeyManager.fromMnemonic(mnemonic, kmParams, None, TimeUtil.now)

    val directXpub = direct.getRootXPub

    val api = BIP39KeyManager
      .initializeWithEntropy(aesPasswordOpt = KeyManagerTestUtil.aesPasswordOpt,
                             entropy = mnemonic.toEntropy,
                             bip39PasswordOpt = None,
                             kmParams = kmParams)
      .getOrElse(fail())

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
      BIP39KeyManager.fromMnemonic(mnemonic,
                                   kmParams,
                                   Some(bip39Pw),
                                   TimeUtil.now)

    val directXpub = direct.getRootXPub

    val api = BIP39KeyManager
      .initializeWithEntropy(aesPasswordOpt = KeyManagerTestUtil.aesPasswordOpt,
                             mnemonic.toEntropy,
                             Some(bip39Pw),
                             kmParams)
      .getOrElse(fail())

    val apiXpub = api.getRootXPub

    assert(apiXpub == directXpub,
           s"We don't have initialization symmetry between our constructors!")

    //we should be able to derive the same child xpub
    assert(api.deriveXPub(hdAccount) == direct.deriveXPub(hdAccount))
  }

  it must "write a mnemonic key manager to disk then initialize a key manager from a params" in {
    val kmParams = buildParams()
    val bip39Pw = KeyManagerTestUtil.bip39Password
    val direct =
      BIP39KeyManager.fromMnemonic(mnemonic,
                                   kmParams,
                                   Some(bip39Pw),
                                   TimeUtil.now)

    val decryptedMnemonic = DecryptedMnemonic(mnemonic, direct.creationTime)
    val password = AesPassword.fromNonEmptyString("password")
    WalletStorage.writeSeedToDisk(kmParams.seedPath,
                                  decryptedMnemonic.encrypt(password))

    val directXpub = direct.getRootXPub
    val api =
      BIP39KeyManager
        .fromParams(kmParams, Some(password), Some(bip39Pw))
        .getOrElse(fail())

    val apiXpub = api.getRootXPub

    assert(apiXpub == directXpub,
           s"We don't have initialization symmetry between our constructors!")

    //we should be able to derive the same child xpub
    assert(api.deriveXPub(hdAccount) == direct.deriveXPub(hdAccount))
  }

  it must "write a xprv key manager to disk then initialize a key manager from a params" in {
    val kmParams = buildParams()
    val bip39Pw = KeyManagerTestUtil.bip39Password

    val direct =
      BIP39KeyManager.fromMnemonic(mnemonic,
                                   kmParams,
                                   Some(bip39Pw),
                                   TimeUtil.now)
    val seed = BIP39Seed.fromMnemonic(mnemonic, Some(bip39Pw))
    val privVersion = HDUtil.getXprivVersion(kmParams.purpose, kmParams.network)
    val rootExtPrivKey = seed.toExtPrivateKey(privVersion)

    val decryptedXprv =
      DecryptedExtPrivKey(rootExtPrivKey, direct.creationTime)
    val password = AesPassword.fromNonEmptyString("password")
    WalletStorage.writeSeedToDisk(kmParams.seedPath,
                                  decryptedXprv.encrypt(password))

    val directXpub = direct.getRootXPub
    val api =
      BIP39KeyManager
        .fromParams(kmParams, Some(password), Some(bip39Pw))
        .getOrElse(fail())

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
      BIP39KeyManager.fromMnemonic(mnemonic,
                                   kmParams,
                                   Some(bip39Pw),
                                   TimeUtil.now)

    val directXpub = direct.getRootXPub

    val api = BIP39KeyManager
      .initializeWithMnemonic(aesPasswordOpt =
                                KeyManagerTestUtil.aesPasswordOpt,
                              mnemonic,
                              Some(bip39Pw),
                              kmParams)
      .getOrElse(fail())

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
      BIP39KeyManager.fromMnemonic(mnemonic,
                                   kmParams,
                                   Some(bip39Pw),
                                   TimeUtil.now)
    val withPasswordXpub = withPassword.getRootXPub

    val noPassword =
      BIP39KeyManager.fromMnemonic(mnemonic, kmParams, None, TimeUtil.now)

    val noPwXpub = noPassword.getRootXPub

    assert(
      withPasswordXpub != noPwXpub,
      s"A key manager with a BIP39 password should not generate the same xpub as a key manager without a password!")

  }

  it must "return a mnemonic not found if we have not initialized the key manager" in {
    val kmParams = buildParams()
    val kmE = BIP39KeyManager.fromParams(kmParams = kmParams,
                                         passwordOpt =
                                           Some(BIP39KeyManager.badPassphrase),
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
      aesPasswordOpt = KeyManagerTestUtil.aesPasswordOpt,
      entropy = badEntropy,
      bip39PasswordOpt = KeyManagerTestUtil.bip39PasswordOpt,
      kmParams = buildParams())

    assert(init == Left(InitializeKeyManagerError.BadEntropy))
  }

  it must "read an existing seed from disk if we call initialize and one already exists" in {
    val seedPath = KeyManagerTestUtil.tmpSeedPath
    val aesPasswordOpt = KeyManagerTestUtil.aesPasswordOpt
    val kmParams =
      keymanagement.KeyManagerParams(seedPath, HDPurposes.SegWit, RegTest)
    val entropy = MnemonicCode.getEntropy256Bits
    val passwordOpt = Some(KeyManagerTestUtil.bip39Password)
    val keyManager = withInitializedKeyManager(aesPasswordOpt = aesPasswordOpt,
                                               kmParams = kmParams,
                                               entropy = entropy,
                                               bip39PasswordOpt = passwordOpt)

    assert(Files.exists(keyManager.kmParams.seedPath),
           s"Seed path must exist after calling withInitializedKeyManager")

    val firstXpub = keyManager.getRootXPub

    //now let's try to initialize again, our xpub should be exactly the same
    val keyManager2E =
      BIP39KeyManager.initialize(aesPasswordOpt = aesPasswordOpt,
                                 kmParams,
                                 bip39PasswordOpt = passwordOpt)
    keyManager2E match {
      case Left(_) =>
        fail(s"Must have been able to intiialize the key manager for test")
      case Right(km2) =>
        assert(km2.getRootXPub == firstXpub)
    }

  }

  it must "fail read an existing seed from disk if it is malformed" in {
    val seedPath = KeyManagerTestUtil.tmpSeedPath
    val aesPasswordOpt = KeyManagerTestUtil.aesPasswordOpt
    val kmParams =
      keymanagement.KeyManagerParams(seedPath, HDPurposes.SegWit, RegTest)
    val entropy = MnemonicCode.getEntropy256Bits
    val passwordOpt = Some(KeyManagerTestUtil.bip39Password)
    val keyManager = withInitializedKeyManager(aesPasswordOpt = aesPasswordOpt,
                                               kmParams = kmParams,
                                               entropy = entropy,
                                               bip39PasswordOpt = passwordOpt)

    assert(Files.exists(keyManager.kmParams.seedPath),
           s"Seed path must exist after calling withInitializedKeyManager")

    // change the data to not be json format
    Files.write(kmParams.seedPath, "now this is the wrong format".getBytes)

    //now let's try to initialize again, it should fail with a JsonParsingError
    val keyManager2E =
      BIP39KeyManager.initialize(aesPasswordOpt,
                                 kmParams,
                                 bip39PasswordOpt = passwordOpt)
    keyManager2E match {
      case Left(InitializeKeyManagerError.FailedToReadWrittenSeed(unlockErr)) =>
        unlockErr match {
          case JsonParsingError(_) => succeed
          case result @ (KeyManagerUnlockError.BadPassword |
              KeyManagerUnlockError.MnemonicNotFound) =>
            fail(
              s"Expected to fail test with ${KeyManagerUnlockError.JsonParsingError} got $result")
        }
      case result @ (Left(_) | Right(_)) =>
        fail(
          s"Expected to fail test with ${KeyManagerUnlockError.JsonParsingError} got $result")
    }

  }

  private def buildParams(): KeyManagerParams = {
    keymanagement.KeyManagerParams(seedPath = KeyManagerTestUtil.tmpSeedPath,
                                   purpose = purpose,
                                   network = MainNet)
  }
}
