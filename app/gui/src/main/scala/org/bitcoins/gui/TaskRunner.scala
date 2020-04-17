package org.bitcoins.gui

import scalafx.application.Platform
import scalafx.scene.Node
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.Alert

/**
  * Runs a background task disabling the `mainView` and main visible `glassPane`.
  * Shows statis using `statusLabel`.
  *
  * Copied from [[https://github.com/scalafx/ScalaFX-Tutorials/blob/master/slick-table/src/main/scala/org/scalafx/slick_table/TaskRunner.scala]]
  */
class TaskRunner(mainView: Node, glassPane: Node) {

  /**
    * Run an operation on a separate thread. Return and wait for its completion,
    * then return result of running that operation.
    *
    * A progress indicator is displayed while running the operation.
    *
    * @param caption name for the thread (useful in debugging) and status displayed
    *                when running the task.
    * @param op      operation to run.
    * @tparam R type of result returned by the operation.
    * @return result returned by operation `op`.
    */
  def run[R](caption: String, op: => R): Unit = {

    def showProgress(progressEnabled: Boolean): Unit = {
      mainView.disable = progressEnabled
      glassPane.visible = progressEnabled
    }

    // Indicate task in progress
    Platform.runLater {
      showProgress(true)
      GlobalData.statusText.value = caption
    }

    val task = new javafx.concurrent.Task[R] {
      override def call(): R = {
        op
      }
      override def succeeded(): Unit = {
        showProgress(false)
        GlobalData.statusText.value = caption + " - Done."
        // Do callback, if defined
      }
      override def failed(): Unit = {

        showProgress(false)
        GlobalData.statusText.value = caption + " - Failed."
        val t = Option(getException)
        t.foreach(_.printStackTrace())
        // Show error message
        val _ = new Alert(AlertType.Error) {
          initOwner(owner)
          title = caption
          headerText = "Operation failed. " + t
            .map("Exception: " + _.getClass)
            .getOrElse("")
          contentText = t.map(_.getMessage).getOrElse("")
        }.showAndWait()
      }
      override def cancelled(): Unit = {
        showProgress(false)
        GlobalData.statusText.value = caption + " - Cancelled."
      }
    }

    val th = new Thread(task, caption)
    th.setDaemon(true)
    th.start()
  }
}
