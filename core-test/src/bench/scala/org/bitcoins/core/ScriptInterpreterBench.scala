package org.bitcoins.core

import org.scalameter.api._
import org.bitcoins.core.script.interpreter.ScriptInterpreter

object ScriptInterpreterBench extends Bench.OfflineReport {
  performance of "ScriptInterpreter" in {
    measure method "run" in {
      ???
    }
  }
}
