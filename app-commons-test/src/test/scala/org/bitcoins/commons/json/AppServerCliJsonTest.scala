package org.bitcoins.commons.json

import org.bitcoins.commons.rpc.LoadWallet
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class AppServerCliJsonTest extends BitcoinSUnitTest {

  behavior of "AppServerCliJson"

  it must "test loadwallet json parsing" in {
    val walletName = "walletName"
    val aesPassword = "aesPassword"
    val bip39Password = "bip39Password"
    val arr1 = ujson.Arr(walletName, aesPassword, bip39Password)
    val parsed1 = LoadWallet.fromJsArr(arr1)
    assert(parsed1.isSuccess)
    assert(parsed1.get.walletNameOpt == Some(walletName))
    assert(
      parsed1.get.passwordOpt.map(_.toStringSensitive) == Some(aesPassword))
    assert(parsed1.get.bip39PasswordOpt == Some(bip39Password))

    val arr2 = ujson.Arr(walletName, aesPassword, ujson.Null)
    val parsed2 = LoadWallet.fromJsArr(arr2)
    assert(parsed2.isSuccess)
    assert(parsed2.get.walletNameOpt == Some(walletName))
    assert(
      parsed2.get.passwordOpt.map(_.toStringSensitive) == Some(aesPassword))
  }

}
