package org.bitcoins.lnd.rpc

import akka.actor.ActorSystem
import akka.grpc.{GrpcClientSettings, SSLContextUtils}
import com.google.protobuf.ByteString
import grizzled.slf4j.Logging
import lnrpc._
import org.bitcoins.commons.jsonmodels.lnd._
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.ln.LnInvoice
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.lnd.rpc.LndRpcClient._
import org.bitcoins.lnd.rpc.config.LndInstance
import org.bitcoins.rpc.util.NativeProcessFactory
import scodec.bits.ByteVector
import walletrpc.WalletKitClient

import java.io.{File, FileInputStream}
import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

/** @param binary Path to lnd executable
  */
class LndRpcClient(val instance: LndInstance, binary: Option[File] = None)(
    implicit system: ActorSystem)
    extends NativeProcessFactory
    with StartStopAsync[LndRpcClient]
    with Logging {

  /** The command to start the daemon on the underlying OS */
  override def cmd: String = binary match {
    case Some(file) =>
      s"$file --lnddir=${instance.datadir.toAbsolutePath}"
    case None => ""
  }

  implicit val executionContext: ExecutionContext = system.dispatcher

  // These need to be lazy so we don't try and fetch
  // the tls certificate before it is generated

  lazy val certStream = new FileInputStream(instance.certFile)

  // Configure the client
  lazy val clientSettings: GrpcClientSettings =
    GrpcClientSettings
      .connectToServiceAt(instance.rpcUri.getHost, instance.rpcUri.getPort)
      .withTrustManager(SSLContextUtils.trustManagerFromStream(certStream))

  // Create a client-side stub for the services
  lazy val lnd: LightningClient = LightningClient(clientSettings)
  lazy val wallet: WalletKitClient = WalletKitClient(clientSettings)
  lazy val unlocker: WalletUnlockerClient = WalletUnlockerClient(clientSettings)

  def genSeed(): Future[GenSeedResponse] = {
    logger.trace("lnd calling genseed")

    val req = GenSeedRequest()
    unlocker
      .genSeed()
      .invoke(req)
  }

  def initWallet(password: String): Future[ByteString] = {
    logger.trace("lnd calling initwallet")

    val passwordByteStr = ByteString.copyFromUtf8(password)

    for {
      seed <- genSeed()
      req = InitWalletRequest(walletPassword = passwordByteStr,
                              cipherSeedMnemonic = seed.cipherSeedMnemonic)
      res <- unlocker.initWallet(req).map(_.adminMacaroon)
    } yield res
  }

  def unlockWallet(password: String): Future[Unit] = {
    logger.trace("lnd calling unlockwallet")

    val byteStrPass = ByteString.copyFromUtf8(password)
    val req: UnlockWalletRequest =
      UnlockWalletRequest(walletPassword = byteStrPass)

    unlocker
      .unlockWallet()
      .addHeader(macaroonKey, instance.macaroon)
      .invoke(req)
      .map(_ => ())
  }

  def getInfo: Future[GetInfoResponse] = {
    logger.trace("lnd calling getinfo")

    val req = GetInfoRequest()

    lnd
      .getInfo()
      .addHeader(macaroonKey, instance.macaroon)
      .invoke(req)
  }

  def lookupInvoice(rHash: ByteVector): Future[Invoice] = {
    logger.trace("lnd calling lookupinvoice")

    val byteStr = ByteString.copyFrom(rHash.toArray)
    val req: PaymentHash = PaymentHash(rHash = byteStr)

    lnd
      .lookupInvoice()
      .addHeader(macaroonKey, instance.macaroon)
      .invoke(req)
  }

  def addInvoice(
      memo: String,
      value: Satoshis,
      expiry: Long): Future[AddInvoiceResult] = {
    val invoice: Invoice =
      Invoice(memo = memo, value = value.toLong, expiry = expiry)

    addInvoice(invoice)
  }

  def addInvoice(
      memo: String,
      value: MilliSatoshis,
      expiry: Long): Future[AddInvoiceResult] = {
    val invoice: Invoice =
      Invoice(memo = memo, valueMsat = value.toLong, expiry = expiry)

    addInvoice(invoice)
  }

  def addInvoice(invoice: Invoice): Future[AddInvoiceResult] = {
    logger.trace("lnd calling addinvoice")

    lnd
      .addInvoice()
      .addHeader(macaroonKey, instance.macaroon)
      .invoke(invoice)
      .map { res =>
        AddInvoiceResult(
          ByteVector(res.rHash.toByteArray),
          LnInvoice.fromString(res.paymentRequest),
          res.addIndex,
          ByteVector(res.paymentAddr.toByteArray)
        )
      }
  }

  def getNewAddress: Future[BitcoinAddress] = {
    logger.trace("lnd calling newaddress")

    val req: NewAddressRequest = NewAddressRequest(
      AddressType.WITNESS_PUBKEY_HASH)

    lnd
      .newAddress()
      .addHeader(macaroonKey, instance.macaroon)
      .invoke(req)
      .map(r => BitcoinAddress.fromString(r.address))
  }

  def listUnspent: Future[Vector[UTXOResult]] = {
    logger.trace("lnd calling listunspent")

    lnd
      .listUnspent()
      .addHeader(macaroonKey, instance.macaroon)
      .invoke(ListUnspentRequest())
      .map(_.utxos.toVector.map { utxo =>
        val outPointOpt = utxo.outpoint.map { out =>
          val txId = DoubleSha256DigestBE(out.txidStr)
          val vout = UInt32(out.outputIndex)
          TransactionOutPoint(txId, vout)
        }

        UTXOResult(BitcoinAddress.fromString(utxo.address),
                   Satoshis(utxo.amountSat),
                   ScriptPubKey(utxo.pkScript),
                   outPointOpt,
                   utxo.confirmations)

      })
  }

  def connectPeer(nodeId: NodeId, addr: InetSocketAddress): Future[Unit] = {
    val lnAddr = LightningAddress(nodeId.hex, addr.toString)

    val request: ConnectPeerRequest = ConnectPeerRequest(Some(lnAddr))

    connectPeer(request)
  }

  def connectPeer(
      nodeId: NodeId,
      addr: InetSocketAddress,
      permanent: Boolean): Future[Unit] = {

    val lnAddr: LightningAddress = LightningAddress(nodeId.hex, addr.toString)

    val request: ConnectPeerRequest =
      ConnectPeerRequest(Some(lnAddr), permanent)

    connectPeer(request)
  }

  def connectPeer(request: ConnectPeerRequest): Future[Unit] = {
    logger.trace("lnd calling connectpeer")

    lnd
      .connectPeer()
      .addHeader(macaroonKey, instance.macaroon)
      .invoke(request)
      .map(_ => ())
  }

  def openChannel(
      nodeId: NodeId,
      fundingAmount: CurrencyUnit,
      satPerByte: SatoshisPerByte,
      privateChannel: Boolean): Future[Option[TransactionOutPoint]] = {
    logger.trace("lnd calling openchannel")

    val request = OpenChannelRequest(ByteString.copyFrom(nodeId.bytes.toArray),
                                     localFundingAmount =
                                       fundingAmount.satoshis.toLong,
                                     satPerByte = satPerByte.toLong,
                                     `private` = privateChannel)

    lnd
      .openChannelSync()
      .addHeader(macaroonKey, instance.macaroon)
      .invoke(request)
      .map { point =>
        point.fundingTxid.fundingTxidStr match {
          case Some(str) =>
            val txId = DoubleSha256DigestBE(str)
            Some(TransactionOutPoint(txId, UInt32(point.outputIndex)))
          case None => None
        }
      }
  }

  def walletBalance(): Future[WalletBalances] = {
    logger.trace("lnd calling walletbalance")

    lnd
      .walletBalance()
      .addHeader(macaroonKey, instance.macaroon)
      .invoke(WalletBalanceRequest())
      .map { bals =>
        WalletBalances(balance = Satoshis(bals.totalBalance),
                       unconfirmedBalance = Satoshis(bals.unconfirmedBalance),
                       confirmedBalance = Satoshis(bals.confirmedBalance))
      }
  }

  def channelBalance(): Future[ChannelBalances] = {
    logger.trace("lnd calling channelbalance")

    lnd
      .channelBalance()
      .addHeader(macaroonKey, instance.macaroon)
      .invoke(ChannelBalanceRequest())
      .map { bals =>
        ChannelBalances(
          localBalance = Satoshis(bals.localBalance.map(_.sat).getOrElse(0L)),
          remoteBalance = Satoshis(bals.remoteBalance.map(_.sat).getOrElse(0L)),
          unsettledLocalBalance =
            Satoshis(bals.unsettledLocalBalance.map(_.sat).getOrElse(0L)),
          unsettledRemoteBalance =
            Satoshis(bals.unsettledRemoteBalance.map(_.sat).getOrElse(0L)),
          pendingOpenLocalBalance =
            Satoshis(bals.pendingOpenLocalBalance.map(_.sat).getOrElse(0L)),
          pendingOpenRemoteBalance =
            Satoshis(bals.pendingOpenRemoteBalance.map(_.sat).getOrElse(0L))
        )
      }
  }

  def sendPayment(invoice: LnInvoice): Future[SendResponse] = {
    val request: SendRequest = SendRequest(paymentRequest = invoice.toString)

    sendPayment(request)
  }

  def sendPayment(
      nodeId: NodeId,
      amount: CurrencyUnit): Future[SendResponse] = {
    val request: SendRequest = SendRequest(
      dest = ByteString.copyFrom(nodeId.bytes.toArray),
      amt = amount.satoshis.toLong)

    sendPayment(request)
  }

  def sendPayment(request: SendRequest): Future[SendResponse] = {
    logger.trace("lnd calling sendpayment")

    lnd
      .sendPaymentSync()
      .addHeader(macaroonKey, instance.macaroon)
      .invoke(request)
  }

  def monitorInvoice(
      rHash: ByteVector,
      interval: FiniteDuration = 1.second,
      maxAttempts: Int = 60): Future[Invoice] = {
    val p: Promise[Invoice] = Promise[Invoice]()
    val attempts = new AtomicInteger(0)
    val runnable = new Runnable() {

      def run(): Unit = {
        val receivedInfoF = lookupInvoice(rHash)

        //register callback that publishes a payment to our actor system's
        //event stream,
        receivedInfoF.foreach { info: Invoice =>
          if (info.state.isSettled) {
            //invoice has been paid, let's publish to event stream
            //so subscribers so the even stream can see that a payment
            //was received
            //we need to create a `PaymentSucceeded`
            system.eventStream.publish(info)

            //complete the promise so the runnable will be canceled
            p.success(info)
          } else if (attempts.incrementAndGet() >= maxAttempts) {
            // too many tries to get info about a payment
            // either Lnd is down or the payment is still in PENDING state for some reason
            // complete the promise with an exception so the runnable will be canceled
            p.failure(new RuntimeException(
              s"LndApi.monitorInvoice() [${instance.datadir}] too many attempts: ${attempts
                .get()} for invoice=${rHash.toHex}"))
          }
        }
      }
    }

    val cancellable =
      system.scheduler.scheduleAtFixedRate(interval, interval)(runnable)

    p.future.onComplete(_ => cancellable.cancel())

    p.future
  }

  /** Starts lnd on the local system.
    */
  override def start(): Future[LndRpcClient] = {
    startBinary().map(_ => this)
  }

  /** Boolean check to verify the state of the client
    * @return Future Boolean representing if client has started
    */
  def isStarted: Future[Boolean] = {
    val p = Promise[Boolean]()

    Try(getInfo.onComplete {
      case Success(_) =>
        p.success(true)
      case Failure(_) =>
        p.success(false)
    })

    p.future
  }

  /** Returns a Future LndRpcClient if able to shut down
    * Lnd instance, inherits from the StartStop trait
    * @return A future LndRpcClient that is stopped
    */
  override def stop(): Future[LndRpcClient] = {
    logger.trace("lnd calling stop daemon")

    val stopF =
      lnd
        .stopDaemon()
        .addHeader(macaroonKey, instance.macaroon)
        .invoke(StopRequest())
        .flatMap(_ => lnd.close())

    for {
      _ <- stopF
      _ <- stopBinary()
      _ <- {
        if (system.name == LndRpcClient.ActorSystemName)
          system.terminate()
        else Future.unit
      }
    } yield this
  }

  /** Checks to see if the client stopped successfully
    * @return
    */
  def isStopped: Future[Boolean] = {
    isStarted.map(started => !started)
  }
}

object LndRpcClient {

  /** The current version we support of Lnd */
  private[bitcoins] val version = "0.12.1"

  /** Key used for adding the macaroon to the gRPC header */
  private[lnd] val macaroonKey = "macaroon"

  /** THe name we use to create actor systems. We use this to know which
    * actor systems to shut down on node shutdown
    */
  private[lnd] val ActorSystemName = "lnd-rpc-client-created-by-bitcoin-s"

  /** Creates an RPC client from the given instance,
    * together with the given actor system. This is for
    * advanced users, wher you need fine grained control
    * over the RPC client.
    */
  def apply(
      instance: LndInstance,
      binary: Option[File] = None): LndRpcClient = {
    implicit val system: ActorSystem = ActorSystem.create(ActorSystemName)
    withActorSystem(instance, binary)
  }

  /** Constructs a RPC client from the given datadir, or
    * the default datadir if no directory is provided
    */
  def withActorSystem(instance: LndInstance, binary: Option[File] = None)(
      implicit system: ActorSystem) = new LndRpcClient(instance, binary)
}
