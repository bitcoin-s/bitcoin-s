package org.bitcoins.wallet.models

import java.sql.SQLException

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.protocol.P2SHAddress
import org.bitcoins.core.script.ScriptType
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.core.hd.SegWitHDPath
import org.bitcoins.wallet.Wallet
import org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.script.P2WPKHWitnessV0

class AddressDAOTest extends BitcoinSWalletTest with WalletDAOFixture {

  behavior of "AddressDAO"

  it should "fail to insert and read an address into the database without a corresponding account" in {
    daos =>
      val addressDAO = daos.addressDAO
      val readF = {
        val addressDb =
          WalletTestUtil.getAddressDb(WalletTestUtil.firstAccountDb)
        addressDAO.create(addressDb)
      }

      recoverToSucceededIf[SQLException](readF)
  }

  it should "insert and read an address into the database with a corresponding account" in {
    daos =>
      val accountDAO = daos.accountDAO
      val addressDAO = daos.addressDAO
      for {
        createdAccount <- {
          val account = WalletTestUtil.firstAccountDb
          accountDAO.create(account)
        }
        createdAddress <- {
          val addressDb = WalletTestUtil.getAddressDb(createdAccount)
          addressDAO.create(addressDb)
        }
        readAddress <- addressDAO.read(createdAddress.address)
      } yield assert(readAddress.contains(createdAddress))
  }
}
