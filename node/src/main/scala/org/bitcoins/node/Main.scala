package org.bitcoins.node

/**
  * Created by chris on 8/29/16.
  */
object Main extends App {

  override def main(args: Array[String]) = {
    /*    val table = TableQuery[BlockHeaderTable]
    val db = Constants.database
    Await.result(Constants.database.run(table.schema.create),3.seconds)
    db.close()*/

    /*
    val gensisBlockHash = TestNetChainParams.genesisBlock.blockHeader.hash
    val startHeader = BlockHeaderSyncActor.StartHeaders(Seq(gensisBlockHash))

    Constants.database.executor*/
    /*    val blockHeaderSyncActor = BlockHeaderSyncActor(Constants.actorSystem,
                                                    Constants.dbConfig,
                                                    Constants.networkParameters)*/
    //blockHeaderSyncActor ! StartAtLastSavedHeader
  }

}
