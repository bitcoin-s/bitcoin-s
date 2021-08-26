package org.bitcoins.rpc.util

import akka.actor.ActorSystem
import org.bitcoins.commons.config.{AppConfig, AppConfigFactoryBase}

/** An AppConfigFactory that has implicit actor systems passed into the datadir */
trait AppConfigFactoryActorSystem[C <: AppConfig]
    extends AppConfigFactoryBase[C, ActorSystem]
