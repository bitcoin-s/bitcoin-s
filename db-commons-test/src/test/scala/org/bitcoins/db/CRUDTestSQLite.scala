package org.bitcoins.db

import org.bitcoins.testkit.db.TestSQLiteDAOFixture

class CRUDTestSQLite extends TestSQLiteDAOFixture {

  it must "successfully create a db row and read it back" in { testDAO =>
    testCreate(testDAO).map(result => assert(result))
  }

  it must "successfully create multiple db row and read them back" in {
    testDAO =>
      testCreateAll(testDAO).map(result => assert(result))
  }

  it must "successfully delete a db row" in { testDAO =>
    testDelete(testDAO).map(result => assert(result))
  }

  it must "successfully delete multiple db rows" in { testDAO =>
    testDeleteAll(testDAO).map(result => assert(result))
  }

  it must "successfully upsert a db row and read it back" in { testDAO =>
    testUpsert(testDAO).map(result => assert(result))
  }

  it must "successfully upsert multiple db row and read them back" in {
    testDAO =>
      testUpsertAll(testDAO).map(result => assert(result))
  }

  it must "successfully update a db row and read it back" in { testDAO =>
    testUpdate(testDAO).map(result => assert(result))
  }

  it must "successfully update multiple db rows and read them back" in {
    testDAO =>
      testUpdateAll(testDAO).map(result => assert(result))
  }
}
