We use [Slick](http://slick.lightbend.com/) as our library for database bindings in bitcoins-spv-node. Slick offers numerous database bindings such as Postgres, MySQL, DB2 etc. Configuration for databases is specified inside of the [application.conf](https://github.com/bitcoin-s/bitcoin-s-spv-node/blob/master/src/main/resources/application.conf#L14-L63) file inside of src/main/resources. If you want to read more about the different configuration options for Slick the documentation is [here](http://slick.lightbend.com/doc/3.1.1/database.html).

Currently we have 4 databases that need to be created for using our application: 
* bitcoins-spv-node
* bitcoins-spv-node-testnet3
* bitcoins-spv-node-regtest
* bitcoins-spv-node-unit-test

Note, that bitcoins-spv-node is for mainnet. Inside our codebase, we have a trait that represents a database binding called [DbConfig](https://github.com/bitcoin-s/bitcoin-s-spv-node/blob/master/src/main/scala/org/bitcoins/spvnode/constant/DbConfig.scala). This is passed around inside of our codebase to specify what database to use. It is best to just use [the configuration inside of Constants](https://github.com/bitcoin-s/bitcoin-s-spv-node/blob/master/src/main/scala/org/bitcoins/spvnode/constant/Constants.scala#L32) instead of passing around the objects inside of DbConfig. This eliminates the chance of having a situation where we have given the wrong database binding for the network we are currently on, for instance giving the TestNet3 database as an arguement when we are on mainnet. 

Here is an example of creating a table in a database. 

```scala
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.models.BlockHeaderTable
import slick.driver.PostgresDriver.api._
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
object Main extends App {
  override def main(args : Array[String]) = {
    val table = TableQuery[BlockHeaderTable]
    val db = Constants.database
    //creates the table in the database
    Await.result(db.run(table.schema.create),3.seconds)
    db.close()
  }
}
```

now if we wanted to drop that same table, we could use this snippet of code: 

```scala 
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.models.BlockHeaderTable
import slick.driver.PostgresDriver.api._
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
object Main extends App {
  override def main(args : Array[String]) = {
    val table = TableQuery[BlockHeaderTable]
    val db = Constants.database
    //drops the table in the database
    Await.result(db.run(table.schema.drop),3.seconds)
    db.close()
  }
}
```

If you want to see how a table is actually represented in Slick, you can look at how we model our BlockHeaderTable, which stores all headers on the network, [here](https://github.com/bitcoin-s/bitcoin-s-spv-node/blob/master/src/main/scala/org/bitcoins/spvnode/models/BlockHeaderTable.scala). For documentation on creating schemas for tables in Slick, you can look [here](http://slick.lightbend.com/doc/3.1.1/schemas.html).

