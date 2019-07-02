package org.bitcoins.wallet.models

import java.sql.SQLException

import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.hd.{HDChainType, SegWitHDPath}
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.script.{P2WPKHWitnessSPKV0, P2WPKHWitnessV0}
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}
import org.bitcoins.wallet.fixtures.AddressDAOFixture

class AddressDAOTest extends BitcoinSWalletTest with AddressDAOFixture {

  // todo: do this with an actual working address
  // todo: with script witness + redeem script
  private def getAddressDb(account: AccountDb): AddressDb = {
    val path = SegWitHDPath(WalletTestUtil.hdCoinType,
                            chainType = HDChainType.External,
                            accountIndex = account.hdAccount.index,
                            addressIndex = 0)
    val pubkey: ECPublicKey = ECPublicKey.freshPublicKey
    val hashedPubkey = CryptoUtil.sha256Hash160(pubkey.bytes)
    val wspk = P2WPKHWitnessSPKV0(pubkey)
    val scriptWitness = P2WPKHWitnessV0(pubkey)
    val address = Bech32Address.apply(wspk, WalletTestUtil.networkParam)

    SegWitAddressDb(path = path,
                    ecPublicKey = pubkey,
                    hashedPubkey,
                    address,
                    scriptWitness)
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
