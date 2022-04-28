package org.bitcoins.server.util

import scala.concurrent.Future

/** A trait used to indicated when different parts of [[BitcoinSAppConfig]] are started */
sealed trait AppConfigMarker

/** This class represents when BitcoinSAppConfig modules are started
  * @param torStartedF this future is completed when all tor dependent modules are fully started
  *                   the reason this is needed is because tor startup time is so variable
  * @see https://github.com/bitcoin-s/bitcoin-s/issues/4210
  */
case class StartedBitcoinSAppConfig(torStartedF: Future[Unit])
    extends AppConfigMarker

case object StoppedBitcoinSAppConfig extends AppConfigMarker
