package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.{CliCommand, ConsoleCli}
import org.bitcoins.gui.util.GUIUtil
import org.bitcoins.gui.{GlobalData, TaskRunner}
import scalafx.beans.property.StringProperty
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Node
import scalafx.scene.control.{Button, TextField}
import scalafx.scene.layout.{HBox, Priority, VBox}

import scala.util.{Failure, Success}

class DLCDialogContainer[T <: CliCommand](
    caption: String,
    nestedView: Node,
    producer: CliCommandProducer[T],
    taskRunner: TaskRunner,
    filename: String = "") {

  private val returnValue = new StringProperty()

  private val textField = new TextField {
    text <== StringProperty(caption)
  }

  private val executeButton = new Button("Execute") {
    onAction = _ => {
      val command = producer.getCliCommand()
      disable = true // Disable button while processing task
      taskRunner.run(
        caption = caption,
        op = {
          ConsoleCli.exec(command, GlobalData.consoleCliConfig) match {
            case Success(commandReturn) =>
              // Could fire a success callback here
              disable = false
              returnValue.value = commandReturn
            case Failure(_) =>
              // TaskRunner handles modal error and logging
              disable = false
          }
        }
      )
    }
  }

  private val greenCheck = GUIUtil.getGreenCheck()
  greenCheck.visible <== returnValue.isNotEmpty

  private val outputTF = new TextField {
    disable <== returnValue.isEmpty
    text <== returnValue
    editable = false
  }

  val toClipboardButton = GUIUtil.getCopyToClipboardButton(returnValue)

  private val outputHBox = new HBox {
    alignment = Pos.Center
    children = Seq(outputTF, toClipboardButton)
  }

  val toFileButton = new Button("Save to File...") {
    disable <== returnValue.isEmpty
    onAction = _ =>
      GUIUtil.showSaveDialog(filename, Some(returnValue.value), None)
  }

  private val buttonFirstRow = new HBox {
    hgrow = Priority.Always
    alignment = Pos.CenterRight
    spacing = 10
    children = Seq(executeButton)
  }

  private val buttonSecondRow = new HBox {
    padding = Insets(10, 0, 10, 0)
    hgrow = Priority.Always
    alignment = Pos.CenterRight
    spacing = 10
    children = Seq(greenCheck, outputHBox, toFileButton)
  }

  private val buttonBox = new VBox {
    styleClass += "dialog-button-group"
    padding = Insets(10)
    hgrow = Priority.Always
    alignment = Pos.CenterRight
    children = Seq(buttonFirstRow, buttonSecondRow)
  }

  val view = new VBox {
    children = Seq(textField, nestedView, buttonBox)
  }

}
