package org.bitcoins.lnd.rpc

import akka.actor.ActorSystem
import akka.grpc.{GrpcClientSettings, SSLContextUtils}
import com.google.protobuf.ByteString
import grizzled.slf4j.Logging
import io.grpc.{CallCredentials, Metadata}
import lnrpc._
import org.bitcoins.commons.jsonmodels.lnd._
import org.bitcoins.commons.util.NativeProcessFactory
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.ln.LnInvoice
import org.bitcoins.core.protocol.ln.LnTag.PaymentHashTag
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput,
  Transaction => Tx
}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.core.wallet.fee.{SatoshisPerKW, SatoshisPerVirtualByte}
import org.bitcoins.crypto._
import org.bitcoins.lnd.rpc.LndRpcClient._
import org.bitcoins.lnd.rpc.LndUtils._
import org.bitcoins.lnd.rpc.config.{LndInstance, LndInstanceLocal}
import scodec.bits._
import signrpc._
import walletrpc.{
  FinalizePsbtRequest,
  LeaseOutputRequest,
  ListLeasesRequest,
  ReleaseOutputRequest,
  SendOutputsRequest,
  WalletKitClient
}

import java.io.{File, FileInputStream}
import java.net.InetSocketAddress
import java.util.concurrent.Executor
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

/** @param binaryOpt Path to lnd executable
  */
class LndRpcClient(val instance: LndInstance, binaryOpt: Option[File] = None)(
    implicit system: ActorSystem)
    extends NativeProcessFactory
    with StartStopAsync[LndRpcClient]
    with Logging {
  instance match {
    case _: LndInstanceLocal =>
      require(binaryOpt.isDefined,
              s"Binary must be defined with a local instance of lnd")
  }

  /** The command to start the daemon on the underlying OS */
  override def cmd: String = instance match {
    case local: LndInstanceLocal =>
      s"${binaryOpt.get} --lnddir=${local.datadir.toAbsolutePath}"
  }

  implicit val executionContext: ExecutionContext = system.dispatcher

  // These need to be lazy so we don't try and fetch
  // the tls certificate before it is generated

  private[this] lazy val certStream = new FileInputStream(instance.certFile)

  private lazy val callCredentials = new CallCredentials {

    def applyRequestMetadata(
        requestInfo: CallCredentials.RequestInfo,
        appExecutor: Executor,
        applier: CallCredentials.MetadataApplier
    ): Unit = {
      appExecutor.execute(() => {
        val metadata = new Metadata()
        val key =
          Metadata.Key.of(macaroonKey, Metadata.ASCII_STRING_MARSHALLER)
        metadata.put(key, instance.macaroon)
        applier(metadata)
      })
    }

    def thisUsesUnstableApi(): Unit = ()
  }

  // Configure the client
  private lazy val clientSettings: GrpcClientSettings =
    GrpcClientSettings
      .connectToServiceAt(instance.rpcUri.getHost, instance.rpcUri.getPort)
      .withTrustManager(SSLContextUtils.trustManagerFromStream(certStream))
      .withCallCredentials(callCredentials)

  // Create a client-side stub for the services
  lazy val lnd: LightningClient = LightningClient(clientSettings)
  lazy val wallet: WalletKitClient = WalletKitClient(clientSettings)
  lazy val unlocker: WalletUnlockerClient = WalletUnlockerClient(clientSettings)
  lazy val signer: SignerClient = SignerClient(clientSettings)

  def genSeed(): Future[GenSeedResponse] = {
    logger.trace("lnd calling genseed")

    val req = GenSeedRequest()
    unlocker
      .genSeed(req)
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
      .unlockWallet(req)
      .map(_ => ())
  }

  def getInfo: Future[GetInfoResponse] = {
    logger.trace("lnd calling getinfo")

    val req = GetInfoRequest()

    lnd.getInfo(req)
  }

  def nodeId: Future[NodeId] = {
    getInfo.map(info => NodeId(info.identityPubkey))
  }

  def lookupInvoice(rHash: PaymentHashTag): Future[Invoice] = {
    logger.trace("lnd calling lookupinvoice")

    val req: PaymentHash = PaymentHash(rHash = rHash.bytes)

    lnd.lookupInvoice(req)
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
      .addInvoice(invoice)
      .map { res =>
        AddInvoiceResult(
          PaymentHashTag(Sha256Digest(res.rHash)),
          LnInvoice.fromString(res.paymentRequest),
          res.addIndex,
          res.paymentAddr
        )
      }
  }

  def getNewAddress: Future[BitcoinAddress] = {
    logger.trace("lnd calling newaddress")

    val req: NewAddressRequest = NewAddressRequest(
      AddressType.WITNESS_PUBKEY_HASH)

    lnd
      .newAddress(req)
      .map(r => BitcoinAddress.fromString(r.address))
  }

  def listUnspent: Future[Vector[UTXOResult]] = {
    val request = ListUnspentRequest(0, Int.MaxValue)
    listUnspent(request)
  }

  def listUnspent(request: ListUnspentRequest): Future[Vector[UTXOResult]] = {
    logger.trace("lnd calling listunspent")

    lnd
      .listUnspent(request)
      .map(_.utxos.toVector.map { utxo =>
        val outPointOpt = utxo.outpoint.map { out =>
          val txId = DoubleSha256DigestBE(out.txidStr)
          val vout = UInt32(out.outputIndex)
          TransactionOutPoint(txId, vout)
        }

        val spkBytes = ByteVector.fromValidHex(utxo.pkScript)

        UTXOResult(BitcoinAddress.fromString(utxo.address),
                   Satoshis(utxo.amountSat),
                   ScriptPubKey.fromAsmBytes(spkBytes),
                   outPointOpt,
                   utxo.confirmations)

      })
  }

  def connectPeer(nodeId: NodeId, addr: InetSocketAddress): Future[Unit] = {
    val lnAddr =
      LightningAddress(nodeId.hex, s"${addr.getHostName}:${addr.getPort}")

    val request: ConnectPeerRequest = ConnectPeerRequest(Some(lnAddr))

    connectPeer(request)
  }

  def connectPeer(
      nodeId: NodeId,
      addr: InetSocketAddress,
      permanent: Boolean): Future[Unit] = {
    val lnAddr: LightningAddress =
      LightningAddress(nodeId.hex, s"${addr.getHostName}:${addr.getPort}")

    val request: ConnectPeerRequest =
      ConnectPeerRequest(Some(lnAddr), permanent)

    connectPeer(request)
  }

  def connectPeer(request: ConnectPeerRequest): Future[Unit] = {
    logger.trace("lnd calling connectpeer")

    lnd
      .connectPeer(request)
      .map(_ => ())
  }

  def isConnected(nodeId: NodeId): Future[Boolean] = {
    listPeers().map { peers =>
      peers.exists(p => NodeId(p.pubKey) == nodeId)
    }
  }

  def listPeers(): Future[Vector[Peer]] = {
    logger.trace("lnd calling listpeers")

    val request: ListPeersRequest = ListPeersRequest()

    lnd
      .listPeers(request)
      .map(_.peers.toVector)
  }

  def openChannel(
      nodeId: NodeId,
      fundingAmount: CurrencyUnit,
      satPerVByte: SatoshisPerVirtualByte,
      privateChannel: Boolean): Future[Option[TransactionOutPoint]] = {
    val request = OpenChannelRequest(
      nodePubkey = nodeId.bytes,
      localFundingAmount = fundingAmount.satoshis.toLong,
      satPerVbyte = satPerVByte.toLong,
      `private` = privateChannel
    )

    openChannel(request)
  }

  def openChannel(
      nodeId: NodeId,
      fundingAmount: CurrencyUnit,
      pushAmt: CurrencyUnit,
      satPerVByte: SatoshisPerVirtualByte,
      privateChannel: Boolean): Future[Option[TransactionOutPoint]] = {
    val request = OpenChannelRequest(
      nodePubkey = nodeId.bytes,
      localFundingAmount = fundingAmount.satoshis.toLong,
      pushSat = pushAmt.satoshis.toLong,
      satPerVbyte = satPerVByte.toLong,
      `private` = privateChannel
    )

    openChannel(request)
  }

  def openChannel(
      request: OpenChannelRequest): Future[Option[TransactionOutPoint]] = {
    logger.trace("lnd calling openchannel")

    lnd
      .openChannelSync(request)
      .map { point =>
        point.fundingTxid.fundingTxidBytes match {
          case Some(bytes) =>
            val txId = DoubleSha256DigestBE(bytes)
            Some(TransactionOutPoint(txId, UInt32(point.outputIndex)))
          case None => None
        }
      }
  }

  def listChannels(request: ListChannelsRequest =
    ListChannelsRequest()): Future[Vector[Channel]] = {
    logger.trace("lnd calling listchannels")

    lnd
      .listChannels(request)
      .map(_.channels.toVector)
  }

  def findChannel(
      channelPoint: TransactionOutPoint): Future[Option[Channel]] = {
    listChannels().map { channels =>
      channels.find(
        _.channelPoint == s"${channelPoint.txId.hex}:${channelPoint.vout.toLong}")
    }
  }

  def findChannel(chanId: Long): Future[Option[Channel]] = {
    listChannels().map { channels =>
      channels.find(_.chanId == chanId)
    }
  }

  def walletBalance(): Future[WalletBalances] = {
    logger.trace("lnd calling walletbalance")

    lnd
      .walletBalance(WalletBalanceRequest())
      .map { bals =>
        WalletBalances(balance = Satoshis(bals.totalBalance),
                       unconfirmedBalance = Satoshis(bals.unconfirmedBalance),
                       confirmedBalance = Satoshis(bals.confirmedBalance))
      }
  }

  def channelBalance(): Future[ChannelBalances] = {
    logger.trace("lnd calling channelbalance")

    lnd
      .channelBalance(ChannelBalanceRequest())
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
    val request: SendRequest =
      SendRequest(dest = nodeId.bytes, amt = amount.satoshis.toLong)

    sendPayment(request)
  }

  def sendPayment(request: SendRequest): Future[SendResponse] = {
    logger.trace("lnd calling sendpayment")

    lnd
      .sendPaymentSync(request)
  }

  def sendOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: SatoshisPerVirtualByte,
      spendUnconfirmed: Boolean): Future[Tx] = {
    sendOutputs(outputs, feeRate.toSatoshisPerKW, spendUnconfirmed)
  }

  def sendOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: SatoshisPerKW,
      spendUnconfirmed: Boolean): Future[Tx] = {

    val request = SendOutputsRequest(satPerKw = feeRate.toLong,
                                     outputs = outputs,
                                     spendUnconfirmed = spendUnconfirmed)
    sendOutputs(request)
  }

  def sendOutputs(request: SendOutputsRequest): Future[Tx] = {
    logger.trace("lnd calling sendoutputs")

    wallet
      .sendOutputs(request)
      .map(res => Tx(res.rawTx))
  }

  def finalizePSBT(psbt: PSBT): Future[PSBT] = {
    val request = FinalizePsbtRequest(psbt.bytes)

    finalizePSBT(request)
  }

  def finalizePSBT(request: FinalizePsbtRequest): Future[PSBT] = {
    logger.trace("lnd calling finalizepsbt")

    wallet
      .finalizePsbt(request)
      .map(res => PSBT(res.signedPsbt))
  }

  def computeInputScript(
      tx: Tx,
      inputIdx: Int,
      output: TransactionOutput): Future[(ScriptSignature, ScriptWitness)] = {
    val signDescriptor =
      SignDescriptor(output = Some(output),
                     sighash = HashType.sigHashAll.num.toInt,
                     inputIndex = inputIdx)

    computeInputScript(tx, Vector(signDescriptor)).map(_.head)
  }

  def computeInputScript(
      tx: Tx,
      signDescriptors: Vector[SignDescriptor]): Future[
    Vector[(ScriptSignature, ScriptWitness)]] = {
    val request: SignReq =
      SignReq(tx.bytes, signDescriptors)

    computeInputScript(request)
  }

  def computeInputScript(
      request: SignReq): Future[Vector[(ScriptSignature, ScriptWitness)]] = {
    logger.trace("lnd calling computeinputscript")

    signer.computeInputScript(request).map { res =>
      res.inputScripts.map { script =>
        val scriptSig = ScriptSignature.fromAsmBytes(script.sigScript)
        val witness = ScriptWitness(script.witness.reverse.toVector)

        (scriptSig, witness)
      }.toVector
    }
  }

  def listLeases(): Future[Vector[UTXOLease]] = {
    listLeases(ListLeasesRequest())
  }

  def listLeases(request: ListLeasesRequest): Future[Vector[UTXOLease]] = {
    logger.trace("lnd calling listleases")

    wallet
      .listLeases(request)
      .map(_.lockedUtxos.toVector.map { lease =>
        val txId = DoubleSha256DigestBE(lease.outpoint.get.txidBytes)
        val vout = UInt32(lease.outpoint.get.outputIndex)
        val outPoint = TransactionOutPoint(txId, vout)
        UTXOLease(lease.id, outPoint, lease.expiration)
      })
  }

  def leaseOutput(
      outpoint: TransactionOutPoint,
      leaseSeconds: Long): Future[Long] = {
    val outPoint =
      OutPoint(outpoint.txId.bytes, outputIndex = outpoint.vout.toInt)

    val request = LeaseOutputRequest(id = LndRpcClient.leaseId,
                                     outpoint = Some(outPoint),
                                     expirationSeconds = leaseSeconds)

    leaseOutput(request)
  }

  /** LeaseOutput locks an output to the given ID, preventing it from being available for any future coin selection attempts.
    * The absolute time of the lock's expiration is returned.
    * The expiration of the lock can be extended by successive invocations of this RPC.
    * @param request LeaseOutputRequest
    * @return Unix timestamp for when the lease expires
    */
  def leaseOutput(request: LeaseOutputRequest): Future[Long] = {
    logger.trace("lnd calling leaseoutput")

    wallet.leaseOutput(request).map(_.expiration)
  }

  def releaseOutput(outpoint: TransactionOutPoint): Future[Unit] = {
    val outPoint =
      OutPoint(outpoint.txId.bytes, outputIndex = outpoint.vout.toInt)

    val request =
      ReleaseOutputRequest(id = LndRpcClient.leaseId, outpoint = Some(outPoint))

    releaseOutput(request)
  }

  def releaseOutput(request: ReleaseOutputRequest): Future[Unit] = {
    logger.trace("lnd calling releaseoutput")

    wallet.releaseOutput(request).map(_ => ())
  }

  /** Broadcasts the given transaction
    * @return None if no error, otherwise the error string
    */
  def publishTransaction(tx: Tx): Future[Option[String]] = {
    logger.trace("lnd calling publishtransaction")

    val request = walletrpc.Transaction(tx.bytes)

    wallet
      .publishTransaction(request)
      .map { res =>
        if (res.publishError.isEmpty) None
        else Some(res.publishError)
      }
  }

  def getTransaction(txId: DoubleSha256DigestBE): Future[Option[TxDetails]] = {
    // Idk why they don't have a separate function to just get one tx
    getTransactions.map(_.find(_.txId == txId))
  }

  def getTransactions: Future[Vector[TxDetails]] = {
    getTransactions(GetTransactionsRequest())
  }

  def getTransactions(startHeight: Int): Future[Vector[TxDetails]] = {
    getTransactions(startHeight, -1)
  }

  def getTransactions(
      startHeight: Int,
      endHeight: Int): Future[Vector[TxDetails]] = {
    getTransactions(GetTransactionsRequest(startHeight, endHeight))
  }

  def getTransactions(
      request: GetTransactionsRequest): Future[Vector[TxDetails]] = {
    logger.trace("lnd calling gettransactions")

    lnd
      .getTransactions(request)
      .map {
        _.transactions.map { details =>
          val blockHashOpt = if (details.blockHash.isEmpty) {
            None
          } else Some(DoubleSha256DigestBE(details.blockHash))

          val addrs =
            details.destAddresses.map(BitcoinAddress.fromString).toVector

          TxDetails(
            txId = DoubleSha256DigestBE(details.txHash),
            amount = Satoshis(details.amount),
            numConfirmations = details.numConfirmations,
            blockHashOpt = blockHashOpt,
            blockHeight = details.blockHeight,
            timeStamp = details.timeStamp,
            totalFees = Satoshis(details.totalFees),
            destAddresses = addrs,
            tx = Tx(details.rawTxHex),
            label = details.label
          )
        }.toVector
      }
  }

  def monitorInvoice(
      rHash: PaymentHashTag,
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
            p.failure(
              new RuntimeException(
                s"LndApi.monitorInvoice() [$instance] too many attempts: ${attempts
                  .get()} for invoice=${rHash.hash.hex}"))
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
        .stopDaemon(StopRequest())
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

  /** Lease id should be unique per application
    * this is the sha256 of "lnd bitcoin-s"
    */
  val leaseId: ByteString =
    hex"8c45ee0b90e3afd0fb4d6f39afa3c5d551ee5f2c7ac2d06820ed3d16582186d2"

  /** The current version we support of Lnd */
  private[bitcoins] val version = "0.13.1"

  /** Key used for adding the macaroon to the gRPC header */
  private[lnd] val macaroonKey = "macaroon"

  /** THe name we use to create actor systems. We use this to know which
    * actor systems to shut down on node shutdown
    */
  private[lnd] val ActorSystemName = "lnd-rpc-client-created-by-bitcoin-s"

  /** Creates an RPC client from the given instance,
    * together with the given actor system. This is for
    * advanced users, where you need fine grained control
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
