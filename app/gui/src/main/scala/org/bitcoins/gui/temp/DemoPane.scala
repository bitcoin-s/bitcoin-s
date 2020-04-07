package org.bitcoins.gui.temp

import javafx.event.{ActionEvent, EventHandler}
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.gui.GlobalData
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{Button, Label, TextField}
import scalafx.scene.layout.{BorderPane, GridPane}

import scala.util.Try

class DemoPane() {
  private val statusLabel = new Label {
    maxWidth = Double.MaxValue
    padding = Insets(0, 10, 10, 10)
    text <== GlobalData.statusText
  }

  private val resultField = new TextField {
    editable = false
    text = "Enter a scalar below to receive a point"
    prefWidth = 600
    alignment = Pos.Center
  }

  private val scalarField = new TextField {
    prefWidth = 600
    alignment = Pos.Center
  }

  private val button = new Button {
    text = "Compute Point"
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        val privKeyT = Try(ECPrivateKey.fromHex(scalarField.text()))
        val pubKeyT = privKeyT.map(_.publicKey).map(_.hex)
        resultField.text = pubKeyT.getOrElse("Invalid scalar, please try again")
      }
    }
    alignmentInParent = Pos.Center
  }

  private val scalarLabel = new Label {
    text = "Scalar:"
    alignmentInParent = Pos.Center
  }

  private val gridPane = new GridPane {
    hgap = 10
    alignment = Pos.Center

    add(scalarLabel, 0, 0)
    add(resultField, 1, 0)
    add(scalarField, 0, 1)
    add(button, 1, 1)
  }

  val borderPane: BorderPane = new BorderPane {
    center = gridPane
    bottom = statusLabel
  }
}
