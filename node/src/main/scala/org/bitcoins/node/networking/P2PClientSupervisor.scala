package org.bitcoins.node.networking

import akka.actor.SupervisorStrategy._
import akka.actor.{Actor, OneForOneStrategy, Props}
import org.bitcoins.node.P2PLogger
import org.bitcoins.node.util.BitcoinSNodeUtil

class P2PClientSupervisor extends Actor with P2PLogger {

  override val supervisorStrategy: OneForOneStrategy =
    OneForOneStrategy() { case e: Throwable =>
      logger.error("Error while processing messages", e)
      Stop
    }

  def receive: Receive = { case props: Props =>
    /* actors to be supervised need to built withing this context this creates an actor using props and sends back
    the ActorRef */
    sender() ! context.actorOf(props,
                               name =
                                 BitcoinSNodeUtil.createActorName(getClass))
  }
}
