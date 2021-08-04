package org.bitcoins.gui

import org.bitcoins.cli.CliCommand.ZipDataDir
import org.bitcoins.cli.ConsoleCli
import org.bitcoins.gui.settings.Themes
import scalafx.application.Platform
import scalafx.scene.control._
import scalafx.scene.input.{KeyCode, KeyCodeCombination, KeyCombination}
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter

import java.io.File
import scala.util.Properties

object AppMenuBar {

  def menuBar(model: WalletGUIModel): MenuBar = {
    val menuBar = new MenuBar {
      menus = List(new FileMenu().fileMenu,
                   new ViewMenu().viewMenu,
                   new HelpMenu(model).helpMenu)
    }
    // Use MacOS native menuing
    if (Properties.isMac)
      menuBar.useSystemMenuBarProperty.set(true)
    menuBar
  }
}

private class FileMenu() {

  private val backup: MenuItem = new MenuItem("_Save Backup") {
    mnemonicParsing = true
    onAction = _ => {
      val zipExtensionFilter = new ExtensionFilter("zip", "*.zip")
      val allExtensionFilter = new ExtensionFilter("All Files", "*")
      val fileChooser = new FileChooser() {
        extensionFilters.addAll(zipExtensionFilter, allExtensionFilter)
        selectedExtensionFilter = zipExtensionFilter
        initialDirectory = new File(Properties.userHome)
        initialFileName = "bitcoin-s-backup.zip"
      }
      val chosenFileOpt = Option(fileChooser.showSaveDialog(null))
      chosenFileOpt match {
        case Some(chosenFile) =>
          ConsoleCli.exec(ZipDataDir(chosenFile.toPath),
                          GlobalData.consoleCliConfig)
          ()
        case None => // User canceled in dialog
      }
    }
  }

  private val quit: MenuItem = new MenuItem("_Quit") {
    mnemonicParsing = true
    accelerator =
      new KeyCodeCombination(KeyCode.Q,
                             KeyCombination.ShortcutDown
      ) // Ctrl/Cmd + Q
    onAction = _ => Platform.exit()
  }

  val fileMenu: Menu =
    new Menu("_File") {
      mnemonicParsing = true
      items = List(backup, quit)
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
