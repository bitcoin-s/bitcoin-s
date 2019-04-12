package org.bitcoins.wallet.fixtures

import org.bitcoins.wallet.models.UTXOSpendingInfoDAO
import org.bitcoins.wallet.util.BitcoinSWalletTest
import org.scalatest._

trait UtxoDAOFixture
    extends AsyncFlatSpec
    with DAOFixture
    with BitcoinSWalletTest { this: Suite =>

  val utxoDAO: UTXOSpendingInfoDAO = UTXOSpendingInfoDAO(dbConfig)
  daoAccumulator += utxoDAO.asInstanceOf[DAOFixture.HasTable]

}
