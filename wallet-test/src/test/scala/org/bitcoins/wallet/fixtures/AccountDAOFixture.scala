package org.bitcoins.wallet.fixtures

import org.bitcoins.wallet.models.AccountDAO
import org.bitcoins.wallet.util.BitcoinSWalletTest
import org.scalatest._

trait AccountDAOFixture
    extends AsyncFlatSpec
    with DAOFixture
    with BitcoinSWalletTest {
  val accountDAO = AccountDAO(dbConfig)
  daoAccumulator += accountDAO.asInstanceOf[DAOFixture.HasTable]
}
