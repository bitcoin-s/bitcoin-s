package sqlite.wallet.migration

import org.bitcoins.core.api.wallet.db.ScriptPubKeyDb
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.flywaydb.core.api.migration.{BaseJavaMigration, Context}

class V14__compute_spk_hashes extends BaseJavaMigration {

  override def migrate(context: Context): Unit = {
    val selectStatement = context.getConnection.createStatement()
    try {
      val rows = selectStatement.executeQuery(
        "SELECT id, script_pub_key FROM pub_key_scripts")
      while (rows.next()) {
        val id = rows.getLong(1)
        val hex = rows.getString(2)
        val spk = ScriptPubKey(hex)
        val hash = ScriptPubKeyDb.hash(spk)
        val updateStatement = context.getConnection.prepareStatement(
          "UPDATE pub_key_scripts SET hash=? WHERE id=?")
        updateStatement.setString(1, hash.hex)
        updateStatement.setLong(2, id)
        try {
          updateStatement.executeUpdate()
        } finally updateStatement.close()
      }
    } finally selectStatement.close()
  }

}
