package org

package bitcoins.wallet.fixtures

import org.bitcoins.wallet.models.MnemonicCodeDAO
import org.bitcoins.wallet.util.BitcoinSWalletTest
import org.scalatest._

// this uses the stupid way of doing fixtures
// seeing as we're going to store the mnemonic
// as a flat file and not a DB this suffices
// for now
trait MnemonicDAOFixture
    extends fixture.AsyncFlatSpec
    with DAOFixture
    with BitcoinSWalletTest {
  val mnemonicDAO = MnemonicCodeDAO(dbConfig)
  daoAccumulator += mnemonicDAO.asInstanceOf[DAOFixture.HasTable]
}
