package org.bitcoins.gui.dialog

import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.util.GUIUtil
import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{ButtonType, Dialog, Hyperlink, Label, TextField}
import scalafx.scene.layout.{HBox, Priority, VBox}
import scalafx.stage.{Modality, Window}

object FundingTransactionDialog {

  private val MAX_WIDTH = 400

  def show(
      parentWindow: Window,
      fundingTxId: String,
      refundDateTime: String,
      oracleLink: String,
      rebroadcast: Boolean = false): Unit = {
    val dialog = new Dialog[Unit]() {
      initOwner(parentWindow)
      initModality(Modality.None)
      title = "Funding Transaction Broadcast"
    }
    if (rebroadcast) dialog.title = "Funding Transaction Rebroadcast"

    dialog.dialogPane().buttonTypes = Seq(ButtonType.Close)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets

    dialog.dialogPane().content = new VBox {
      padding = Insets(20)
      alignment = Pos.Center
      spacing = 15
      children = Seq(
        new Label("You successfully entered a DLC with your counterparty!"),
        new HBox {
          alignment = Pos.CenterLeft
          spacing = 5
          maxWidth = MAX_WIDTH
          children = Seq(new Label("Funding Transaction Id"),
                         new TextField {
                           text = fundingTxId
                           editable = false
                           hgrow = Priority.Always
                         })
        },
        new Hyperlink("View transaction on mempool.space") {
          onAction =
            _ => GUIUtil.openUrl(GlobalData.buildMempoolSpaceTxUrl(fundingTxId))
        },
        new Hyperlink("View transaction on Blockstream Explorer") {
          onAction = _ =>
            GUIUtil.openUrl(
              GlobalData.buildBlockstreamExplorerTxUrl(fundingTxId))
        },
        new Label(
          "It will take a few seconds for the transaction to show up in the block explorer. Refresh your browser tab momentarily if the transaction is not immediately available.") {
          wrapText = true
          maxWidth = MAX_WIDTH
        },
        new Label(
          s"You can execute the refund clause of your DLC at ${refundDateTime}."),
        new Hyperlink("View Oracle Announcement") {
          onAction = _ => {
            GUIUtil.openUrl(oracleLink)
          }
        }
      )
    }

    val _ = dialog.show()
  }
}
