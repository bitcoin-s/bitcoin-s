package org.bitcoins.gui.dlc.dialog

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc.{
  DLCPayoutCurve,
  NumericContractDescriptor,
  OutcomePayoutPoint,
  RoundingIntervals
}
import org.bitcoins.core.protocol.tlv.OracleAnnouncementTLV
import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.dlc.DLCPlotUtil
import org.bitcoins.gui.util.GUIUtil.setNumericInput
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Node
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, HBox, VBox}
import scalafx.stage.Window

import scala.util.{Failure, Success, Try}

object InitNumericContractDialog {

  def showAndWait(parentWindow: Window): Option[
    (Satoshis, NumericContractDescriptor, Option[OracleAnnouncementTLV])] = {
    val dialog =
      new Dialog[Option[(
          Satoshis,
          NumericContractDescriptor,
          Option[OracleAnnouncementTLV])]]() {
        initOwner(parentWindow)
        title = "Initialize Demo Oracle"
        headerText = "Enter contract interpolation points"
      }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets
    dialog.resizable = true

    val baseTF = new TextField() {
      text = "2"
    }

    val numDigitsTF = new TextField()

    val totalCollateralTF = new TextField() {
      promptText = "Satoshis"
    }
    setNumericInput(baseTF)
    setNumericInput(numDigitsTF)
    setNumericInput(totalCollateralTF)

    val announcementTF = new TextField() {
      promptText = "(optional)"
    }

    val pointMap: scala.collection.mutable.Map[
      Int,
      (TextField, TextField, CheckBox)] =
      scala.collection.mutable.Map.empty

    var nextPointRow: Int = 2
    val pointGrid: GridPane = new GridPane {
      alignment = Pos.Center
      padding = Insets(top = 10, right = 10, bottom = 10, left = 10)
      hgap = 5
      vgap = 5

      add(new Label("Outcome"), 0, 0)
      add(new Label("Payout"), 1, 0)
      add(new Label("Endpoint"), 2, 0)
    }

    def addPointRow(
        xOpt: Option[String] = None,
        yOpt: Option[String] = None): Unit = {

      val xTF = new TextField() {
        promptText = "Outcome (base 10)"
      }
      xOpt match {
        case Some(value) =>
          xTF.text = value
        case None => ()
      }
      val yTF = new TextField() {
        promptText = "Satoshis"
      }
      yOpt match {
        case Some(value) =>
          yTF.text = value
        case None => ()
      }
      val endPointBox = new CheckBox() {
        selected = true
      }
      setNumericInput(xTF)
      setNumericInput(yTF)

      val row = nextPointRow
      val _ = pointMap.put(row, (xTF, yTF, endPointBox))

      pointGrid.add(xTF, 0, row)
      pointGrid.add(yTF, 1, row)
      pointGrid.add(new Label("Endpoint:"), 2, row)
      pointGrid.add(endPointBox, 3, row)

      nextPointRow += 1
      dialog.dialogPane().getScene.getWindow.sizeToScene()
    }

    addPointRow(Some("0"), Some("0"))
    addPointRow()

    val addPointButton: Button = new Button("+") {
      onAction = _ => addPointRow()
    }

    val roundingMap: scala.collection.mutable.Map[Int, (TextField, TextField)] =
      scala.collection.mutable.Map.empty

    var nextRoundingRow: Int = 2
    val roundingGrid: GridPane = new GridPane {
      alignment = Pos.Center
      padding = Insets(top = 10, right = 10, bottom = 10, left = 10)
      hgap = 5
      vgap = 5

      add(new Label("Outcome"), 0, 0)
      add(new Label("Rounding Level"), 1, 0)
    }

    def addRoundingRow(): Unit = {

      val outcomeTF = new TextField() {
        promptText = "Outcome (base 10)"
      }
      val roundingLevelTF = new TextField() {
        promptText = "Satoshis"
      }
      setNumericInput(outcomeTF)
      setNumericInput(roundingLevelTF)

      val row = nextRoundingRow
      val _ = roundingMap.put(row, (outcomeTF, roundingLevelTF))

      roundingGrid.add(outcomeTF, 0, row)
      roundingGrid.add(roundingLevelTF, 1, row)

      nextRoundingRow += 1
      dialog.dialogPane().getScene.getWindow.sizeToScene()
    }

    addRoundingRow()
    addRoundingRow()

    val addRoundingRowButton: Button = new Button("+") {
      onAction = _ => addRoundingRow()
    }

    def getContractInfo: Try[(Satoshis, NumericContractDescriptor)] = {
      Try {
        val numDigits = numDigitsTF.text.value.toInt
        val totalCollateral = Satoshis(totalCollateralTF.text.value.toLong)

        val points = pointMap.values.toVector
        val outcomesValuePoints = points.flatMap { case (xTF, yTF, checkBox) =>
          if (xTF.text.value.nonEmpty && yTF.text.value.nonEmpty) {
            val x = xTF.text.value.toLong
            val y = yTF.text.value.toLong
            Some(OutcomePayoutPoint(x, Satoshis(y), checkBox.selected.value))
          } else {
            None
          }
        }
        val roundingIntervalStartsTF = roundingMap.values.toVector
        val roundingIntervalsStarts = roundingIntervalStartsTF.flatMap {
          case (outcomeTF, roundingModTF) =>
            if (
              outcomeTF.text.value.nonEmpty && roundingModTF.text.value.nonEmpty
            ) {
              val outcome = outcomeTF.text.value.toLong
              val roundingMod = roundingModTF.text.value.toLong
              Some(
                RoundingIntervals.IntervalStart(BigDecimal(outcome),
                                                roundingMod))
            } else None
        }

        val sorted = outcomesValuePoints.sortBy(_.outcome)
        require(sorted == outcomesValuePoints, "Must be sorted by outcome")

        val func = DLCPayoutCurve(outcomesValuePoints)
        (totalCollateral,
         NumericContractDescriptor(func,
                                   numDigits,
                                   RoundingIntervals(roundingIntervalsStarts)))
      }
    }

    def getRoundingIntervals: RoundingIntervals = {
      val roundingIntervalsT = Try {
        roundingMap.values.toVector.flatMap {
          case (outcomeTF, roundingLevelTF) =>
            if (
              outcomeTF.text.value.nonEmpty && roundingLevelTF.text.value.nonEmpty
            ) {
              val outcome = BigDecimal(outcomeTF.text.value.toDouble)
              val level = roundingLevelTF.text.value.toLong
              Some((outcome, level))
            } else {
              None
            }
        }
      }

      roundingIntervalsT match {
        case Failure(_) => RoundingIntervals.noRounding
        case Success(intervals) =>
          RoundingIntervals(intervals.map { case (firstOutcome, roundingMod) =>
            RoundingIntervals.IntervalStart(firstOutcome, roundingMod)
          })
      }
    }

    val vBoxContent: VBox = new VBox() {
      padding = Insets(20, 10, 10, 10)
      spacing = 10
      alignment = Pos.Center

      val eventDataGrid: GridPane = new GridPane {
        padding = Insets(top = 10, right = 10, bottom = 10, left = 10)
        hgap = 5
        vgap = 5

        add(new Label("Base"), 0, 0)
        add(baseTF, 1, 0)
        add(new Label("Num Digits"), 0, 1)
        add(numDigitsTF, 1, 1)
        add(new Label("Total Collateral"), 0, 2)
        add(totalCollateralTF, 1, 2)
        add(new Label("Announcement"), 0, 3)
        add(announcementTF, 1, 3)
      }

      val outcomes: Node = new VBox {
        alignment = Pos.Center

        val label: HBox = new HBox {
          alignment = Pos.Center
          spacing = 10
          children = Vector(new Label("Points"), addPointButton)
        }
        children = Vector(label, pointGrid)
      }

      val roundingIntervals: VBox = new VBox {
        alignment = Pos.Center

        val label: HBox = new HBox {
          alignment = Pos.Center
          spacing = 10
          children =
            Vector(new Label("Rounding Intervals"), addRoundingRowButton)
        }
        children = Vector(label, roundingGrid)
      }

      val roundingPane: TitledPane = new TitledPane() {
        text = "Rounding Info"
        content = roundingIntervals
      }

      val roundingAccordion: Accordion = new Accordion() {
        panes = Vector(roundingPane)
      }

      val previewGraphButton: Button = new Button("Preview Graph") {
        onAction = _ => {
          getContractInfo match {
            case Failure(_) => ()
            case Success((totalCollateral, descriptor)) =>
              DLCPlotUtil.plotCETsWithOriginalCurve(base = 2,
                                                    descriptor.numDigits,
                                                    descriptor.outcomeValueFunc,
                                                    totalCollateral,
                                                    getRoundingIntervals)
              ()
          }
        }
      }

      children = Vector(eventDataGrid,
                        new Separator(),
                        outcomes,
                        roundingAccordion,
                        previewGraphButton)
    }

    dialog.dialogPane().content = new ScrollPane() {
      content = vBoxContent
    }
    // Enable/Disable OK button depending on whether all data was entered.
    val okButton = dialog.dialogPane().lookupButton(ButtonType.OK)
    // Simple validation that sufficient data was entered
    okButton.disable <== baseTF.text.isEmpty || totalCollateralTF.text.isEmpty

    Platform.runLater(numDigitsTF.requestFocus())

    // When the OK button is clicked, convert the result to a CreateDLCOffer.
    dialog.resultConverter = dialogButton =>
      if (dialogButton == ButtonType.OK) {
        getContractInfo match {
          case Failure(exception) => throw exception
          case Success((sats, contractInfo)) =>
            val announcementStr = announcementTF.text.value
            val announcementOpt = if (announcementStr.nonEmpty) {
              Some(OracleAnnouncementTLV(announcementStr))
            } else None
            Some((sats, contractInfo, announcementOpt))
        }
      } else None

    dialog.showAndWait() match {
      case Some(
            Some(
              (totalCollateral: Satoshis,
               descriptor: NumericContractDescriptor,
               announcementOpt: Option[_]))) =>
        Some(
          (totalCollateral,
           descriptor,
           announcementOpt.asInstanceOf[Option[OracleAnnouncementTLV]]))
      case Some(_) | None => None
    }
  }
}
