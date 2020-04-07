package org.bitcoins.gui.settings

import javafx.event.{ActionEvent, EventHandler}
import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.WalletGUI.stage
import scalafx.scene.control.CheckBox
import scalafx.scene.layout.StackPane

class SettingsPane {

  private val themeCheckBox = new CheckBox {
    text = "Dark Theme"
    selected = GlobalData.defaultDarkTheme
    onAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent): Unit = {
        if (!selected.value) {
          stage.scene.value.getStylesheets.removeAll("/themes/dark-theme.css")
        } else {
          stage.scene.value.getStylesheets.add("/themes/dark-theme.css")
        }
        ()
      }
    }
  }

  val view: StackPane = new StackPane {
    children = Seq(
      themeCheckBox
    )
  }
}
