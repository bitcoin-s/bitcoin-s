package org.bitcoins.core.hd

import org.bitcoins.testkitcore.gen.HDGenerators
import org.bitcoins.testkitcore.wallet.WalletTestUtil
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class HDAccountTest extends BitcoinSUnitTest {

  behavior of "HDAccount"

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = {
    generatorDrivenConfigNewCode
  }

  val defaultAcct = WalletTestUtil.defaultHdAccount
  val defaultPath = defaultAcct.path
  it must "determine if a bip32 path is the same as a given HDAccount" in {
    val isSameDefault = HDAccount.isSameAccount(defaultPath, defaultAcct)
    assert(isSameDefault, s"Must have symmetry")
  }

  it must "fail if the given path is shorter than how BIP44 defines accounts" in {
    val missingLast = defaultPath.dropRight(1)

    val isNotSame = !HDAccount.isSameAccount(missingLast, defaultAcct)

    assert(
      isNotSame,
      s"If we drop the last element from the defualt path, we are not in the same account anymore")
  }

  it must "fail if we modify the last element in the path" in {
    val newLast = defaultPath.last.copy(index = Int.MaxValue)
    val modifiedLast = defaultPath.updated(defaultPath.length - 1, newLast)

    val isNotSame = !HDAccount.isSameAccount(modifiedLast, defaultAcct)

    assert(isNotSame,
           s"We should have the same account if we modify the account index")
  }

  it must "succeed if we add an arbitrary element onto the end of the path" in {
    val extraNode = defaultPath.:+(BIP32Node(0, true))

    val isSame = HDAccount.isSameAccount(extraNode, defaultAcct)

    assert(
      isSame,
      s"If we add an extra element onto the path, we are still in the same account")
  }

  it must "fail with the empty path" in {
    val empty = BIP32Path.empty
    val isNotSame = !HDAccount.isSameAccount(empty, defaultAcct)
    assert(isNotSame)
  }

  it must "have symmetry for isSameAccount with all hdaccount" in {
    forAll(HDGenerators.hdAccount) { acct =>
      val path = acct.path
      assert(HDAccount.isSameAccount(path, acct))
    }
  }

  it must "not taken an arbitrary path and arbitrary account and find them in the same account" in {
    forAll(HDGenerators.hdAccount, HDGenerators.bip32Path) {
      case (acct, path) =>
        assert(!HDAccount.isSameAccount(path, acct))
    }
  }
}
