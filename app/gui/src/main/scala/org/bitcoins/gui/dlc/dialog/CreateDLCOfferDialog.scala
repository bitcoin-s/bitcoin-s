package org.bitcoins.gui.dlc.dialog

import grizzled.slf4j.Logging
import org.bitcoins.cli.CliCommand.CreateDLCOffer
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.dlc.DLCPlotUtil
import org.bitcoins.gui.dlc.dialog.CreateDLCOfferDialog.getNumericContractInfo
import org.bitcoins.gui.util.GUIUtil._
import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.Window

import java.time.ZoneOffset
import scala.collection._
import scala.util.{Failure, Success, Try}

class CreateDLCOfferDialog extends Logging {

  def showAndWait(parentWindow: Window): Option[CreateDLCOffer] = {
    val dialog = new Dialog[Option[CreateDLCOffer]]() {
      initOwner(parentWindow)
      title = "Create DLC Offer"
      headerText = "Enter DLC Contract Details"
    }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets
    dialog.resizable = true

    val fields: mutable.Map[Int, (TextField, TextField)] = mutable.Map.empty

    var announcementDetailsShown = false
    val announcementOrContractInfoTF = new TextField()
    var decompOpt: Option[DigitDecompositionEventDescriptorV0TLV] = None

    var nextRow: Int = 3
    val gridPane = new GridPane {
      alignment = Pos.Center
      padding = Insets(top = 10, right = 10, bottom = 10, left = 10)
      hgap = 5
      vgap = 5

      add(
        new Label("Oracle Announcement/Contract Info") {
          tooltip = Tooltip(
            "An oracle announcement or a contract info can be entered here.")
          tooltip.value.setShowDelay(new javafx.util.Duration(100))
        },
        0,
        0
      )
      add(announcementOrContractInfoTF, 1, 0)
    }
    val detailsGridPane = new GridPane {
      alignment = Pos.Center
      padding = Insets(top = 10, right = 10, bottom = 10, left = 10)
      hgap = 5
      vgap = 5
    }
    val vbox = new VBox(gridPane) {
      alignment = Pos.Center
      spacing = 10
    }

    def addEnumOutcomeRow(
        outcomeText: String,
        amtOpt: Option[Satoshis]): Unit = {

      val outcomeTF = new TextField() {
        text = outcomeText
        disable = true
        editable = false
      }
      val amtTF = new TextField() {
        promptText = "Satoshis"
        tooltip = Tooltip(
          s"""Amount you will win if the oracle signs for "$outcomeText".""")
        tooltip.value.setShowDelay(new javafx.util.Duration(100))
        text = amtOpt match {
          case Some(amt) => numberFormatter.format(amt.toLong)
          case None      => ""
        }
      }
      setNumericInput(amtTF)

      val row = nextRow
      val _ = fields.put(row, (outcomeTF, amtTF))

      gridPane.add(outcomeTF, 0, row)
      gridPane.add(amtTF, 1, row)

      nextRow += 1
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
      vgap = 0

      add(new Label("Outcome"), 0, 0)
      add(
        new Label("Payout") {
          tooltip = Tooltip(
            "Amount you will win if the oracle signs for the given outcome.")
          tooltip.value.setShowDelay(new javafx.util.Duration(100))
        },
        1,
        0
      )
      add(new Label("Endpoint") {
            minWidth = 100
          },
          2,
          0)
    }

    def addPointRow(
        xOpt: Option[String] = None,
        yOpt: Option[String] = None,
        isEndPoint: Boolean = true,
        row: Int = nextPointRow): Unit = {

      val xTF = new TextField() {
        promptText = "Outcome"
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
        selected = isEndPoint
        alignmentInParent = Pos.Center
      }
      setNumericInput(xTF)
      setNumericInput(yTF)

      pointMap.put(row, (xTF, yTF, endPointBox))

      pointGrid.add(xTF, 0, row)
      pointGrid.add(yTF, 1, row)
      pointGrid.add(endPointBox, 2, row)

      nextPointRow += 1
      dialog.dialogPane().getScene.getWindow.sizeToScene()
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

    def addRoundingRow(
        outcomeOpt: Option[Long],
        levelOpt: Option[Satoshis]): Unit = {

      val outcomeTF = new TextField() {
        promptText = "Outcome"
        text = outcomeOpt.map(_.toString).getOrElse("")
      }
      val roundingLevelTF = new TextField() {
        promptText = "Satoshis"
        text = levelOpt match {
          case Some(level) => level.toLong.toString
          case None        => ""
        }
      }
      setNumericInput(outcomeTF)
      setNumericInput(roundingLevelTF)

      val row = nextRoundingRow
      roundingMap.put(row, (outcomeTF, roundingLevelTF))

      roundingGrid.add(outcomeTF, 0, row)
      roundingGrid.add(roundingLevelTF, 1, row)

      nextRoundingRow += 1
      dialog.dialogPane().getScene.getWindow.sizeToScene()
    }

    val addRoundingRowButton: Button = new Button("+") {
      onAction = _ => addRoundingRow(None, None)
    }

    val roundingIntervals = new ScrollPane() {
      content = new VBox {
        alignment = Pos.Center

        val label: HBox = new HBox {
          alignment = Pos.Center
          spacing = 10
          children =
            Vector(new Label("Rounding Intervals"), addRoundingRowButton)
        }
        children = Vector(label, roundingGrid)
      }
    }

    val roundingPane: TitledPane = new TitledPane() {
      text = "Rounding Info"
      content = roundingIntervals
    }

    val roundingAccordion: Accordion = new Accordion() {
      panes = Vector(roundingPane)
    }

    val feeRateTF = new TextField() {
      text = GlobalData.feeRate.toLong.toString
      promptText = "(optional)"
    }

    val collateralTF = new TextField() {
      promptText = "Satoshis"
    }
    setNumericInput(collateralTF)

    val refundDatePicker = new DatePicker()

    val previewGraphButton: Button = new Button("Preview Graph") {
      onAction = _ => {
        getNumericContractInfo(
          decompOpt,
          pointMap.toVector.sortBy(_._1).map(_._2),
          roundingMap.toVector.sortBy(_._1).map(_._2)) match {
          case Failure(_) => ()
          case Success((totalCollateral, descriptor)) =>
            DLCPlotUtil.plotCETsWithOriginalCurve(base = 2,
                                                  descriptor.numDigits,
                                                  descriptor.outcomeValueFunc,
                                                  totalCollateral,
                                                  descriptor.roundingIntervals)
            ()
        }
      }
    }

    def addRemainingFields(): Unit = {
      var nextRow = 0
      detailsGridPane.add(
        new Label("Your Collateral") {
          tooltip =
            Tooltip("How much funds you will be putting up for this DLC.")
          tooltip.value.setShowDelay(new javafx.util.Duration(100))
        },
        0,
        nextRow
      )
      detailsGridPane.add(collateralTF, 1, nextRow)
      nextRow += 1

      detailsGridPane.add(
        new Label("Fee Rate (sats/vbyte)") {
          tooltip = Tooltip(
            "Fee rate to be used for both funding and closing transactions.")
          tooltip.value.setShowDelay(new javafx.util.Duration(100))
        },
        0,
        nextRow
      )
      detailsGridPane.add(feeRateTF, 1, nextRow)
      nextRow += 1

      detailsGridPane.add(
        new Label("Refund Date") {
          tooltip = Tooltip(
            "If no oracle signatures are given, the DLC can be refunded after this date.")
          tooltip.value.setShowDelay(new javafx.util.Duration(100))
        },
        0,
        nextRow
      )
      detailsGridPane.add(refundDatePicker, 1, nextRow)
      nextRow += 1

      vbox.children.add(detailsGridPane)
      ()
    }

    announcementOrContractInfoTF.onKeyTyped = _ => {
      if (!announcementDetailsShown) {
        Try(
          OracleAnnouncementV0TLV.fromHex(
            announcementOrContractInfoTF.text.value)) match {
          case Failure(_) =>
            Try(
              ContractInfoV0TLV.fromHex(
                announcementOrContractInfoTF.text.value)) match {
              case Failure(_) => ()
              case Success(contractInfo) =>
                contractInfo.oracleInfo match {
                  case OracleInfoV0TLV(announcement) =>
                    onAnnouncementEntered(
                      announcement.asInstanceOf[OracleAnnouncementV0TLV],
                      Some(contractInfo))
                  case multi: MultiOracleInfoTLV =>
                    // todo display all oracles
                    onAnnouncementEntered(
                      multi.oracles.head.asInstanceOf[OracleAnnouncementV0TLV],
                      Some(contractInfo))
                }
            }
          case Success(announcement) =>
            onAnnouncementEntered(announcement, None)
        }
      }
    }

    def onAnnouncementEntered(
        announcement: OracleAnnouncementV0TLV,
        contractInfoOpt: Option[ContractInfoV0TLV]): Unit = {
      announcementDetailsShown = true
      announcementOrContractInfoTF.disable = true
      gridPane.add(new Label("Event Id"), 0, 1)
      gridPane.add(new TextField() {
                     text = announcement.eventTLV.eventId
                     editable = false
                   },
                   1,
                   1)

      announcement.eventTLV.eventDescriptor match {
        case EnumEventDescriptorV0TLV(outcomes) =>
          gridPane.add(new Label("Outcomes"), 0, 2)
          gridPane.add(new Label("Values"), 1, 2)
          contractInfoOpt match {
            case Some(contractInfo) =>
              contractInfo.contractDescriptor match {
                case ContractDescriptorV0TLV(outcomes) =>
                  outcomes.foreach(outcome =>
                    addEnumOutcomeRow(outcome._1, Some(outcome._2)))
                case _: ContractDescriptorV1TLV =>
                  throw new RuntimeException(
                    "Got incompatible contract info and announcement")
              }
            case None =>
              outcomes.foreach(str => addEnumOutcomeRow(str.normStr, None))
          }
          nextRow = 3
          addRemainingFields()
        case digitDecomp: UnsignedDigitDecompositionEventDescriptor =>
          decompOpt = Some(digitDecomp)

          val addPointButton: Button = new Button("+") {
            onAction = _ => addPointRow()
          }
          val label: HBox = new HBox {
            alignment = Pos.Center
            spacing = 10
            children = Vector(new Label("Points"), addPointButton)
          }

          contractInfoOpt match {
            case Some(contractInfo) =>
              contractInfo.contractDescriptor match {
                case ContractDescriptorV0TLV(_) =>
                  throw new RuntimeException(
                    "Got incompatible contract info and announcement")
                case descriptor: ContractDescriptorV1TLV =>
                  descriptor.payoutFunction.points.init.foreach { point =>
                    addPointRow(
                      xOpt = Some(numberFormatter.format(point.outcome)),
                      yOpt = Some(numberFormatter.format(point.value.toLong)),
                      isEndPoint = point.isEndpoint)
                  }
                  // handle last specially so user can add more rows
                  val last = descriptor.payoutFunction.points.last

                  addPointRow(xOpt = Some(numberFormatter.format(last.outcome)),
                              yOpt =
                                Some(numberFormatter.format(last.value.toLong)),
                              isEndPoint = last.isEndpoint,
                              row = 9999)
                  nextPointRow -= 1 // do this so the max is the last row

                  // add rounding intervals
                  if (descriptor.roundingIntervals.intervalStarts.nonEmpty) {
                    descriptor.roundingIntervals.intervalStarts.foreach {
                      case (outcome, value) =>
                        addRoundingRow(Some(outcome), Some(value))
                    }
                  } else {
                    // add empty rounding intervals
                    addRoundingRow(None, None)
                    addRoundingRow(None, None)
                  }
              }
            case None =>
              addPointRow(Some("0"))
              addPointRow(Some(numberFormatter.format(digitDecomp.maxNum)),
                          row = 9999)
              nextPointRow -= 1 // do this so the max is the last row

              // add empty rounding intervals
              addRoundingRow(None, None)
              addRoundingRow(None, None)
          }

          vbox.children.addAll(new Separator(),
                               label,
                               pointGrid,
                               roundingAccordion,
                               previewGraphButton)
          nextRow = 4
          addRemainingFields()
        case _: SignedDigitDecompositionEventDescriptor => ()
      }
      dialog.dialogPane().getScene.getWindow.sizeToScene()
    }

    dialog.dialogPane().content = new ScrollPane {
      content = vbox
    }

    def getOracleInfo: Option[OracleInfo] = {
      Try(
        OracleAnnouncementV0TLV.fromHex(
          announcementOrContractInfoTF.text.value)) match {
        case Failure(_) =>
          Try(
            ContractInfoV0TLV.fromHex(
              announcementOrContractInfoTF.text.value)) match {
            case Failure(_) => None
            case Success(contractInfo) =>
              Some(OracleInfo.fromTLV(contractInfo.oracleInfo))
          }
        case Success(announcement) => Some(SingleOracleInfo(announcement))
      }
    }

    // When the OK button is clicked, convert the result to a CreateDLCOffer.
    dialog.resultConverter = dialogButton =>
      if (dialogButton == ButtonType.OK) {
        val oracleInfo = getOracleInfo.get

        val collateralLong =
          numberFormatter.parse(collateralTF.text.value).longValue()
        val collateral = Satoshis(collateralLong)

        val feeRateStr = feeRateTF.text.value

        val feeRateOpt = if (feeRateStr.nonEmpty) {
          Some(SatoshisPerVirtualByte(Satoshis(BigInt(feeRateStr))))
        } else None

        val refundLocktime = {
          val value = refundDatePicker.delegate.getValue
          val instant = value.atStartOfDay(ZoneOffset.UTC).toInstant

          UInt32(instant.getEpochSecond)
        }

        val contractInfo = oracleInfo match {
          case oracleInfo: EnumOracleInfo =>
            val missingOutcomes = fields.values.filter(_._2.text.value.isEmpty)
            if (missingOutcomes.nonEmpty) {
              val missing = missingOutcomes.map(_._1.text.value).mkString(", ")
              throw new RuntimeException(
                s"You missed outcomes $missing. Please enter payouts for these situations")
            }

            val inputs = fields.values.flatMap { case (str, value) =>
              if (str.text.value.nonEmpty && value.text.value.nonEmpty) {
                val amount =
                  numberFormatter.parse(value.text.value).longValue()
                Some((str.text.value, amount))
              } else None
            }
            val contractMap = inputs.map { case (str, value) =>
              EnumOutcome(str) -> Satoshis(value)
            }.toVector

            val descriptor = EnumContractDescriptor(contractMap)

            ContractInfo(descriptor, oracleInfo).toTLV
          case oracleInfo: NumericOracleInfo =>
            getNumericContractInfo(
              decompOpt,
              pointMap.toVector.sortBy(_._1).map(_._2),
              roundingMap.toVector.sortBy(_._1).map(_._2)) match {
              case Failure(exception) => throw exception
              case Success((totalCol, numeric)) =>
                ContractInfo(totalCol, numeric, oracleInfo).toTLV
            }
        }

        Some(
          CreateDLCOffer(
            contractInfo = contractInfo,
            collateral = collateral,
            feeRateOpt = feeRateOpt,
            locktime = UInt32.zero,
            refundLT = refundLocktime
          ))
      } else None

    val result = dialog.showAndWait()

    result match {
      case Some(Some(offer: CreateDLCOffer)) => Some(offer)
      case Some(_) | None                    => None
    }
  }
}

object CreateDLCOfferDialog {

  def getNumericContractInfo(
      decompOpt: Option[DigitDecompositionEventDescriptorV0TLV],
      pointVec: Vector[(TextField, TextField, CheckBox)],
      roundingVec: Vector[(TextField, TextField)]): Try[
    (Satoshis, NumericContractDescriptor)] = {
    decompOpt match {
      case Some(decomp) =>
        Try {
          val numDigits = decomp.numDigits.toInt

          val outcomesValuePoints = pointVec.flatMap {
            case (xTF, yTF, checkBox) =>
              if (xTF.text.value.nonEmpty && yTF.text.value.nonEmpty) {
                val x = numberFormatter.parse(xTF.text.value).longValue()
                val y = numberFormatter.parse(yTF.text.value).longValue()
                Some(
                  OutcomePayoutPoint(x, Satoshis(y), checkBox.selected.value))
              } else {
                None
              }
          }
          val totalCollateral = outcomesValuePoints.map(_.roundedPayout).max

          val roundingIntervalsStarts = roundingVec.flatMap {
            case (outcomeTF, roundingModTF) =>
              if (
                outcomeTF.text.value.nonEmpty && roundingModTF.text.value.nonEmpty
              ) {
                val outcome =
                  numberFormatter.parse(outcomeTF.text.value).doubleValue()
                val roundingMod =
                  numberFormatter.parse(roundingModTF.text.value).longValue()
                Some(
                  RoundingIntervals.IntervalStart(BigDecimal(outcome),
                                                  roundingMod))
              } else None
          }

          val sorted = outcomesValuePoints.sortBy(_.outcome)
          require(sorted == outcomesValuePoints,
                  s"Must be sorted by outcome, got $outcomesValuePoints")

          val func = DLCPayoutCurve(outcomesValuePoints)
          (totalCollateral,
           NumericContractDescriptor(
             func,
             numDigits,
             RoundingIntervals(roundingIntervalsStarts)))
        }
      case None => Failure(new RuntimeException("No announcement"))
    }
  }

}
