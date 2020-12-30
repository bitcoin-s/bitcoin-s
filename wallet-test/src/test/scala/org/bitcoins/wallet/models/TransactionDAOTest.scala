package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.{TransactionDb, TransactionDbHelper}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.testkit.Implicits.GeneratorOps
import org.bitcoins.testkit.core.gen.TransactionGenerators
import org.bitcoins.testkit.fixtures.WalletDAOFixture

class TransactionDAOTest extends WalletDAOFixture {

  val tx: Transaction = TransactionGenerators.realisticTransaction.sampleSome
//  val tx: Transaction = Transaction.fromHex(
//    "1b3cbac1000103de79b2c76d6292042e226601f010052449b2bfa5b0b5ff32baf9975aec79f40491976eef484730440220578c0356738c1250d71987a2f12713ce1c702010bd8057c54455fd0dcc1e8029022060f7db722f1c42a9b8f4e0d3b19d4602d8c1a33079b1aaad352fdd68f03b5987018c2f455691a38655965c796efbfa7f8f73f6b7756e3d535fde0e8c20ea47e148bc4b5954fdd9a90d4947304402201c8a8c3379977268c7cabf4025c5b111c7f78c594307e4c2b235f3451b5d1802022044334af069f2733516e5c68afe52956ad55d391c9be93695116c820c0eb9a31e805141c17a6f18ab8b8013e6db08ad7e7347940340a12a2b77763cb5bbb5b05148f77c2a59525a3adf4849473044022054a5ce2c797c6465f52ddb0e3c3efedbe4df6e20426fd1407062df76e1adbdc802204837e891ff72952542054838040ff2f6bf212c5eaae78275d70bc353d37c693381517051a55202d8e82e99a80549a72004dc90c547b17576a9149bfd3714e4ce825c3fef634a284d97127707d8ad88ac0c7899238b15c864bc63002102b585203f475e17cbdcd942c3d078a57cfa793a7a7f70f488c7468f42d3b4ede451ae67056e1466d000b175636321038d4848e578e6ae223ab824ccda93d51a649bbd62b8e4dd9b00adea6d4006d741ac6768676476a914c9baa3b75097215db3332eacf5fa4e16f08b1fd388ac67512102370d0731cfc84c624510d2e1f926582ebfebf937cfd4ed1159ec3816c5f868592103d6c973de143c66b8b1a7d978b5588c256f94cea6dfe49f6bbadf63f5403f682152ae68686802473045022100dfa70b8f55f20abd4090e178102379b6d91d9ec982cb2100377750f5891c19110220709f5877bdcf8b1630c7e2d2d7e727623dcc4ead5b9fa9a0cd3b85c6ecb2f07e2102898465cec2736301b56790b3cda4be1cab003742665d3ab06ae2abc0d1b1a1a4034830450221009272f1a0fe1eee655feae5908604a83d8fda8aded5fd3dc3796596419d0d4b1f022008fef741932d654f78445ec535a15c5f2d57ea69348f0c47e691a656cf52e4020121021eed98615050ec8940d335a0ce6e5c512d1910c383dd8bf2fdc61a364a93b0e2070476ccfd4eb27500e2df2f6b")

  val txDb: TransactionDb = TransactionDbHelper.fromTransaction(tx)

  it should "insert and read an transaction into the database" in { daos =>
    val txDAO = daos.transactionDAO

    for {
      created <- txDAO.create(txDb)
      _ = assert(created == txDb)
      found <- txDAO.read(txDb.txIdBE)
    } yield {
      assert(found.contains(created))
    }
  }

  it must "find a transaction by txIdBE" in { daos =>
    val txDAO = daos.transactionDAO

    for {
      created <- txDAO.create(txDb)
      found <- txDAO.findByTxId(txDb.txIdBE)
    } yield assert(found.contains(created))
  }

  it must "find a transaction by txId" in { daos =>
    val txDAO = daos.transactionDAO

    for {
      created <- txDAO.create(txDb)
      found <- txDAO.findByTxId(txDb.txId)
    } yield assert(found.contains(created))
  }
}
