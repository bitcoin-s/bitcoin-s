package org.bitcoins.gui.dialog

import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.util.GUIUtil
import scalafx.Includes._
import scalafx.geometry.Pos
import scalafx.scene.control.{ButtonType, Dialog, Hyperlink, Label, TextField}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.stage.{Modality, Window}

object TransactionSentDialog {

  def show(parentWindow: Window, txId: String): Unit = {
    val dialog = new Dialog[Unit]() {
      initOwner(parentWindow)
      initModality(Modality.None)
      title = "Transaction Sent"
    }
    dialog.dialogPane().buttonTypes = Seq(ButtonType.Close)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets

    dialog.dialogPane().content = new VBox {
      alignment = Pos.Center
      spacing = 10
      minHeight = 100
      children = Seq(
        new HBox {
          alignment = Pos.Center
          spacing = 15
          children = Seq(new Label("Transaction Id"),
                         new TextField {
                           text = txId
                           minWidth = 200
                           editable = false
                         })
        },
        new Hyperlink("View transaction on mempool.space") {
          onAction =
            _ => GUIUtil.openUrl(GlobalData.buildMempoolSpaceTxUrl(txId))
        },
        new Hyperlink("View transaction on Blockstream Explorer") {
          onAction =
            _ => GUIUtil.openUrl(GlobalData.buildBlockstreamExplorerTxUrl(txId))
        },
        new Label(
          "It will take a few seconds for the transaction to show up in the block explorer. Refresh your browser tab momentarily if the transaction is not immediately available.") {
          wrapText = true
          maxWidth = 285
        }
      )
    }

    val _ = dialog.show()
  }

}
