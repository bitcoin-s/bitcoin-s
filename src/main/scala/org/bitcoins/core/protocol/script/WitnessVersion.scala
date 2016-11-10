package org.bitcoins.core.protocol.script

/**
  * Created by chris on 11/10/16.
  */
sealed trait WitnessVersion

case object WitnessVersion0 extends WitnessVersion

object WitnessVersion {
  def apply(num: Long): WitnessVersion = num match {
    case 0 => WitnessVersion0
    case _ => throw new IllegalArgumentException("Invalid number given to match witness, got: " + num)
  }
}
