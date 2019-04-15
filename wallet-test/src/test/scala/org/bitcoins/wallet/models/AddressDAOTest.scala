package org.bitcoins.wallet.models

import java.sql.SQLException

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.crypto.bip44.{BIP44ChainType, BIP44Path}
import org.bitcoins.core.protocol.P2SHAddress
import org.bitcoins.core.script.ScriptType
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.wallet.fixtures.AddressDAOFixture
import org.bitcoins.wallet.util.{BitcoinSWalletTest, WalletTestUtil}

class AddressDAOTest extends BitcoinSWalletTest with AddressDAOFixture {

  // todo: do this with an actual working address
  // todo: with script witness + redeem script
  private def getAddressDb(account: AccountDb): AddressDb = {
    val path = BIP44Path(WalletTestUtil.bip44Coin,
                         chainType = BIP44ChainType.External,
                         accountIndex = account.bip44Account.index,
                         addressIndex = 0)
    val pubkey: ECPublicKey = ECPublicKey.freshPublicKey
    val hashedPubkey = CryptoUtil.sha256Hash160(pubkey.bytes)
    val address = P2SHAddress(hashedPubkey, RegTest)

    AddressDb(path, pubkey, hashedPubkey, address, None, ScriptType.SCRIPTHASH)
  }

  behavior of "AddressDAO"

  it should "fail to insert and read an address into the database without a corresponding account" in {
    daos =>
      val (_, addressDAO) = daos
      val readF = {
        val addressDb = getAddressDb(WalletTestUtil.firstAccountDb)
        addressDAO.create(addressDb)
      }

      recoverToSucceededIf[SQLException](readF)
  }

  it should "insert and read an address into the database with a corresponding account" in {
    daos =>
      val (accountDAO, addressDAO) = daos
      for {
        createdAccount <- {
          val account = WalletTestUtil.firstAccountDb
          accountDAO.create(account)
        }
        createdAddress <- {
          val addressDb = getAddressDb(createdAccount)
          addressDAO.create(addressDb)
        }
        readAddress <- addressDAO.read(createdAddress.address)
      } yield assert(readAddress.contains(createdAddress))
  }
}
