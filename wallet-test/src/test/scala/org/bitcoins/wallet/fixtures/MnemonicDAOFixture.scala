package org

package bitcoins.wallet.fixtures

import org.bitcoins.wallet.models.MnemonicCodeDAO
import org.bitcoins.wallet.util.BitcoinSWalletTest
import org.scalatest._

trait MnemonicDAOFixture
    extends AsyncFlatSpec
    with DAOFixture
    with BitcoinSWalletTest {
  val mnemonicDAO = MnemonicCodeDAO(dbConfig)
  daoAccumulator += mnemonicDAO.asInstanceOf[DAOFixture.HasTable]
}
