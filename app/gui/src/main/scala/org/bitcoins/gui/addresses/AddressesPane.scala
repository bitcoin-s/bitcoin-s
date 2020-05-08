package org.bitcoins.gui.addresses

import javafx.event.{ActionEvent, EventHandler}
import scalafx.Includes._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.gui.{GlobalData, TaskRunner}
import scalafx.beans.property.StringProperty
import scalafx.geometry.Insets
import scalafx.scene.Parent
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{
  Alert,
  Button,
  ButtonBar,
  Label,
  SelectionMode,
  TableColumn,
  TableView
}
import scalafx.scene.layout.{BorderPane, VBox}

class AddressesPane(glassPane: VBox) {
  private val statusLabel = new Label {
    maxWidth = Double.MaxValue
    padding = Insets(0, 10, 10, 10)
    text <== GlobalData.statusText
  }

  val model = new AddressesPaneModel()

  private val table: TableView[BitcoinAddress] = {
    val addressColumn = new TableColumn[BitcoinAddress, String] {
      text = "Address"
      cellValueFactory = { address =>
        new StringProperty(address, "address", address.value.value)
      }
      prefWidth = 400
    }

    new TableView[BitcoinAddress](model.addresses) {
      columns ++= Seq(addressColumn)
      margin = Insets(10, 0, 10, 0)
      selectionModel().selectionMode = SelectionMode.Multiple
    }
  }

  model.selectedAddresses = table.selectionModel().selectedItems

  model.setUp()

  private val newAddressButton = new Button {
    text = "Get New Address"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = model.onGetNewAddress()
    }
  }

  private val displaySelectedAddressesButton = new Button {
    text = "Display Selected Addresses"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        val _ = new Alert(AlertType.Information) {
          initOwner(owner)
          title = "Selected Addresses"
          contentText =
            model.selectedAddresses.toVector.map(_.value).mkString("\n")
        }.showAndWait()
      }
    }
  }

  private val buttonBar = new ButtonBar {
    buttons = Seq(newAddressButton, displaySelectedAddressesButton)
  }

  val view: Parent = new BorderPane {
    top = buttonBar
    center = table
    bottom = statusLabel
  }

  private val taskRunner = new TaskRunner(table, glassPane)
  model.taskRunner = taskRunner
}
