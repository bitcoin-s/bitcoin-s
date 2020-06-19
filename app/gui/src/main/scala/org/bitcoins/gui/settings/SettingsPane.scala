package org.bitcoins.gui.settings

import javafx.event.{ActionEvent, EventHandler}
import org.bitcoins.gui.GlobalData
import scalafx.scene.control.CheckBox
import scalafx.scene.layout.StackPane

class SettingsPane {

  private val themeCheckBox = new CheckBox {
    text = "Dark Theme"
    selected = GlobalData.darkThemeEnabled
    onAction = new EventHandler[ActionEvent] {

      override def handle(event: ActionEvent): Unit = {
        if (!selected.value) {
          Themes.DarkTheme.undoTheme
        } else {
          Themes.DarkTheme.applyTheme
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
