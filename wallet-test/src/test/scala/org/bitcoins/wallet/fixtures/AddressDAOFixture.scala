package org.bitcoins.wallet.fixtures

import org.bitcoins.wallet.models
import org.bitcoins.wallet.models.AddressDAO
import org.bitcoins.wallet.util.BitcoinSWalletTest
import org.scalatest._

trait AddressDAOFixture
    extends AsyncFlatSpec
    with DAOFixture
    with BitcoinSWalletTest {
  this: Suite =>

  val addressDAO = AddressDAO(dbConfig)
  daoAccumulator += addressDAO.asInstanceOf[DAOFixture.HasTable]

}
