package org.bitcoins.gui

import org.bitcoins.gui.settings.Themes
import scalafx.scene.control._
import scalafx.scene.input.{KeyCode, KeyCodeCombination, KeyCombination}

object AppMenuBar {

  def menuBar(model: WalletGUIModel): MenuBar =
    new MenuBar {
      menus = List(new FileMenu().fileMenu,
                   new ViewMenu().viewMenu,
                   new HelpMenu(model).helpMenu)
    }
}

private class FileMenu() {

  private val quit: MenuItem = new MenuItem("_Quit") {
    mnemonicParsing = true
    accelerator =
      new KeyCodeCombination(KeyCode.Q, KeyCombination.ControlDown) // CTRL + Q
    onAction = _ => sys.exit()
  }

  val fileMenu: Menu =
    new Menu("_File") {
      mnemonicParsing = true
      items = List(quit)
    }
}

private class ViewMenu() {

  private val themeToggle: ToggleGroup = new ToggleGroup()

  private val themes: Menu = new Menu("_Themes") {
    mnemonicParsing = true

    private val darkThemeToggle: RadioMenuItem = new RadioMenuItem(
      "_Dark Theme") {
      toggleGroup = themeToggle
      selected = GlobalData.darkThemeEnabled
      id = "dark"
    }

    private val lightThemeToggle: RadioMenuItem = new RadioMenuItem(
      "_Light Theme") {
      toggleGroup = themeToggle
      selected = !GlobalData.darkThemeEnabled
      id = "light"
    }

    items = List(darkThemeToggle, lightThemeToggle)

    onAction = _ => {
      val selectedId = themeToggle.selectedToggle.value
        .asInstanceOf[javafx.scene.control.RadioMenuItem]
        .getId

      selectedId match {
        case "dark" =>
          GlobalData.darkThemeEnabled = true
          Themes.DarkTheme.applyTheme
        case "light" =>
          GlobalData.darkThemeEnabled = false
          Themes.DarkTheme.undoTheme
        case _: String =>
          throw new RuntimeException("Error, this shouldn't be possible")
      }
      ()
    }
  }

  val viewMenu: Menu = new Menu("_View") {
    mnemonicParsing = true
    items = List(themes)
  }
}

private class HelpMenu(model: WalletGUIModel) {

  private val about =
    new MenuItem("_About") {
      accelerator = new KeyCodeCombination(KeyCode.F1) // F1
      onAction = _ => model.onAbout()
    }

  val helpMenu: Menu =
    new Menu("_Help") {
      mnemonicParsing = true
      items = List(about)
    }
}
