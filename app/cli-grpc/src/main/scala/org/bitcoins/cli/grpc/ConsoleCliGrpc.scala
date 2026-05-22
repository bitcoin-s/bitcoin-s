package org.bitcoins.cli.grpc

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.grpc.GrpcClientSettings
import org.bitcoins.cli.CliCommand.{
  AddDLCOffer,
  GetBestBlockHash,
  GetBlockCount,
  GetConnectionCount,
  GetDLCHostAddress,
  GetFilterCount,
  GetFilterHeaderCount,
  GetInfo,
  GetMedianTimePast,
  GetVersion,
  IncomingOffersList,
  OfferSend,
  RemoveDLCOffer,
  ZipDataDir
}
import org.bitcoins.cli.{Config, ConsoleCli}
import org.bitcoins.commons.jsonmodels.BitcoinSServerInfo
import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockHeaderResult
import org.bitcoins.commons.rpc.{
  AcceptDLC,
  AppServerCliCommand,
  CliCommand,
  CliGrpcCommand,
  ContactAdd,
  ContactsList,
  CreateContractInfo,
  DLCContactAdd,
  GetBlockHeader,
  OracleServerCliCommand,
  ServerlessCliCommand
}
import org.bitcoins.commons.rpc.CliCommand.NoCommand
import org.bitcoins.commons.serializers.Picklers
import org.bitcoins.core.api.dlc.wallet.db.{DLCContactDb, IncomingDLCOfferDb}
import org.bitcoins.core.config.BitcoinNetworks
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.tlv.DLCOfferTLV
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.crypto.{DoubleSha256DigestBE, Sha256Digest}
import org.bitcoins.server.grpc.{
  AcceptDlcRequest,
  ChainRoutesClient,
  CommonRoutesClient,
  ContactAddRequest,
  ContactsListRequest,
  ContactsListResponse,
  CreateContractInfoRequest,
  DLCRoutesClient,
  DlcContactAddRequest,
  GetBestBlockHashRequest,
  GetBlockCountRequest,
  GetBlockHeaderRequest,
  GetBlockHeaderResponse,
  GetConnectionCountRequest,
  GetDlcHostAddressRequest,
  GetFilterCountRequest,
  GetFilterHeaderCountRequest,
  GetInfoRequest,
  GetMedianTimePastRequest,
  GetVersionRequest,
  GetVersionResponse,
  GrpcAuth,
  IncomingOfferListResponse,
  IncomingOffersListRequest,
  NodeRoutesClient,
  OfferAddRequest,
  OfferRemoveRequest,
  OfferSendRequest,
  ZipDataDirRequest
}
import scopt.OParser
import ujson.{Null, Num, Str}

import java.time.Instant
import scala.concurrent.Future
import scala.util.{Failure, Success}

object ConsoleCliGrpc {

  def parser: OParser[Unit, Config] = ConsoleCli.parser

  // Global options that consume the following argument
  private val globalOptsWithArg: Set[String] =
    Set("--host", "--rpcport", "--password")

  // scopt's cmd() requires the subcommand token to precede any global options.
  // This reorders args so leading global options are moved after the first
  // non-option token (the command name), preserving the original semantics.
  private def normalizeArgs(args: Vector[String]): Vector[String] = {
    @annotation.tailrec
    def peel(
        remaining: Vector[String],
        globalOpts: Vector[String]
    ): (Vector[String], Vector[String]) =
      remaining match {
        case opt +: value +: tail if globalOptsWithArg.contains(opt) =>
          peel(tail, globalOpts :+ opt :+ value)
        case flag +: tail if flag.startsWith("-") =>
          peel(tail, globalOpts :+ flag)
        case _ => (remaining, globalOpts)
      }

    val (cmdAndArgs, globalOpts) = peel(args, Vector.empty)
    cmdAndArgs ++ globalOpts
  }

  def exec(args: Vector[String])(implicit
      system: ActorSystem): Future[String] = {
    val normalized = normalizeArgs(args)
    OParser.parse(parser, normalized, Config()) match {
      case None =>
        Future.failed(
          new RuntimeException(
            s"Invalid arguments provided. See usage above. args=$normalized"))
      case Some(conf) =>
        conf.command match {
          case c: CliGrpcCommand =>
            exec(c, conf)
          case _: AppServerCliCommand | _: ServerlessCliCommand |
              _: OracleServerCliCommand =>
            Future.failed(
              new RuntimeException(
                s"Command ${conf.command} is not supported in gRPC mode"))
        }

    }
  }

  private def jsValueToString(value: ujson.Value): String = {
    value match {
      case Str(string)             => string
      case Num(num) if num.isWhole => num.toLong.toString
      case Num(num)                => num.toString
      case rest: ujson.Value       => rest.render(2)
    }
  }

  def exec(command: CliGrpcCommand, config: Config)(implicit
      system: ActorSystem
  ): Future[String] = {
    import system.dispatcher

    val baseSettings = GrpcClientSettings
      .connectToServiceAt(ConsoleCli.host, config.rpcPort)
      .withTls(false)
    val clientSettings =
      if (config.rpcPassword.isEmpty) {
        baseSettings
      } else {
        baseSettings.withCallCredentials(
          GrpcAuth
            .basicCallCredentials(config.rpcPassword))
      }

    val commonClient = CommonRoutesClient(clientSettings)
    val chainClient = ChainRoutesClient(clientSettings)
    val nodeClient = NodeRoutesClient(clientSettings)
    val dlcClient = DLCRoutesClient(clientSettings)

    val responseF = command match {
      case GetVersion =>
        commonClient.getVersion(GetVersionRequest()).map(formatGetVersion)
      case ZipDataDir(path) =>
        commonClient
          .zipDataDir(ZipDataDirRequest(path = path.toString))
          .map(_ => "")
      case GetInfo =>
        chainClient.getInfo(GetInfoRequest()).map(formatGetInfo)
      case GetBlockCount =>
        chainClient
          .getBlockCount(GetBlockCountRequest())
          .map(r => jsValueToString(Num(r.count)))
      case GetFilterCount =>
        chainClient
          .getFilterCount(GetFilterCountRequest())
          .map(r => jsValueToString(Num(r.count)))
      case GetFilterHeaderCount =>
        chainClient
          .getFilterHeaderCount(GetFilterHeaderCountRequest())
          .map(r => jsValueToString(Num(r.count)))
      case GetBestBlockHash =>
        chainClient
          .getBestBlockHash(GetBestBlockHashRequest())
          .map(r => jsValueToString(Str(r.hash)))
      case GetBlockHeader(hash) =>
        chainClient
          .getBlockHeader(GetBlockHeaderRequest(hash = hash.hex))
          .map(formatGetBlockHeader)
      case GetMedianTimePast =>
        chainClient
          .getMedianTimePast(GetMedianTimePastRequest())
          .map(r => r.mediantimepast.toString)
      case GetConnectionCount =>
        nodeClient
          .getConnectionCount(GetConnectionCountRequest())
          .map(r => jsValueToString(Num(r.count)))
      case GetDLCHostAddress =>
        dlcClient
          .getDlcHostAddress(GetDlcHostAddressRequest())
          .map(r => jsValueToString(Str(r.address)))
      case command: AcceptDLC =>
        dlcClient
          .acceptDlc(
            AcceptDlcRequest(
              offer = command.offer.hex,
              peerAddr =
                s"${command.peerAddr.getHostString}:${command.peerAddr.getPort}",
              externalPayoutAddress =
                command.externalPayoutAddressOpt.map(_.toString),
              externalChangeAddress =
                command.externalChangeAddressOpt.map(_.toString)
            ))
          .map(_.acceptHex)
      case command: CreateContractInfo =>
        dlcClient
          .createContractInfo(
            CreateContractInfoRequest(
              announcement = command.announcementTLV.hex,
              totalCollateralSats = command.totalCollateral.satoshis.toLong,
              contractDescriptorHex = command.contractDescriptor.hex
            )
          )
          .map(_.contractInfoHex)
      case IncomingOffersList =>
        dlcClient
          .incomingOffersList(IncomingOffersListRequest())
          .map(formatOffersList)
      case command: AddDLCOffer =>
        dlcClient
          .offerAdd(
            OfferAddRequest(
              offer = command.offer.hex,
              peer = Some(command.peer),
              message = Some(command.message)
            ))
          .map(_.offerHash)
      case command: RemoveDLCOffer =>
        dlcClient
          .offerRemove(OfferRemoveRequest(hash = command.offerHash.hex))
          .map(_.offerHash)
      case command: OfferSend =>
        dlcClient
          .offerSend(
            OfferSendRequest(
              offerOrTempContractId = command.offer,
              peerAddress = command.peerAddress,
              message = command.message
            ))
          .map(_.tempContractId)
      case ContactsList =>
        dlcClient.contactsList(ContactsListRequest()).map(formatContactsList)
      case command: ContactAdd =>
        dlcClient
          .contactAdd(
            ContactAddRequest(
              alias = command.alias,
              address =
                s"${command.address.getHostString}:${command.address.getPort}",
              memo = command.memo
            ))
          .map(_.result)
      case command: DLCContactAdd =>
        dlcClient
          .dlcContactAdd(
            DlcContactAddRequest(
              dlcId = command.dlcId.hex,
              address =
                s"${command.address.getHostString}:${command.address.getPort}"
            ))
          .map { response =>
            jsValueToString(
              ujson.Obj(
                "dlcId" -> Str(response.dlcId),
                "contactId" -> Str(response.contactId)
              ))
          }
      case NoCommand =>
        Future.failed(
          new IllegalArgumentException("You need to provide a command!"))
      case x: CliCommand =>
        Future.failed(
          new IllegalArgumentException(
            s"Command $x is not supported in gRPC mode"))
    }

    responseF.transformWith {
      case Success(result) =>
        for {
          _ <- commonClient.close()
          _ <- chainClient.close()
          _ <- nodeClient.close()
          _ <- dlcClient.close()
        } yield result
      case Failure(err) =>
        for {
          _ <- commonClient.close()
          _ <- chainClient.close()
          _ <- nodeClient.close()
          _ <- dlcClient.close()
          result <- Future.failed(err)
        } yield result
    }
  }

  private def formatGetVersion(response: GetVersionResponse): String = {
    val version =
      response.version.map(Str.apply).getOrElse(Null)

    jsValueToString(ujson.Obj("version" -> version))
  }

  private def formatGetInfo(
      response: org.bitcoins.server.grpc.GetInfoResponse): String = {
    val info = BitcoinSServerInfo(
      network = BitcoinNetworks.fromString(response.network),
      blockHeight = response.blockHeight,
      blockHash = DoubleSha256DigestBE.fromHex(response.blockHash),
      torStarted = response.torStarted,
      syncing = response.syncing,
      isInitialBlockDownload = response.isInitialBlockDownload
    )
    jsValueToString(info.toJson)
  }

  private def formatGetBlockHeader(response: GetBlockHeaderResponse): String = {
    response.header match {
      case None => "null"
      case Some(header) =>
        val result = GetBlockHeaderResult(
          hash = DoubleSha256DigestBE.fromHex(header.hash),
          confirmations = header.confirmations,
          height = header.height,
          version = header.version,
          versionHex = Int32.fromHex(header.versionHex),
          merkleroot = DoubleSha256DigestBE.fromHex(header.merkleroot),
          // proto uint32 is Scala Int (signed); mask to recover unsigned bits
          time = UInt32(header.time.toLong & 0xffffffffL),
          mediantime = UInt32(header.mediantime.toLong & 0xffffffffL),
          nonce = UInt32(header.nonce.toLong & 0xffffffffL),
          bits = UInt32.fromHex(header.bits),
          difficulty = BigDecimal(header.difficulty),
          chainwork = header.chainwork,
          previousblockhash =
            header.previousblockhash.map(DoubleSha256DigestBE.fromHex),
          nextblockhash =
            header.nextblockhash.map(DoubleSha256DigestBE.fromHex),
          target = header.target
        )
        val json = upickle.default.writeJs(result)(
          using Picklers.getBlockHeaderResultPickler)
        jsValueToString(json)
    }
  }

  private def formatOffersList(response: IncomingOfferListResponse): String = {
    val ios: Seq[IncomingDLCOfferDb] = response.offers.map { offer =>
      IncomingDLCOfferDb(
        hash = Sha256Digest.fromHex(offer.hash),
        receivedAt = Instant.ofEpochSecond(offer.receivedAt),
        peer = offer.peer,
        message = offer.message,
        offerTLV = DLCOfferTLV.fromHex(offer.offerTlvHex)
      )
    }

    val offersJson =
      ios.map(io => upickle.default.writeJs(io)(using Picklers.dlcOfferAddW))
    val json = ujson.Arr.from(offersJson)

    jsValueToString(json)

  }

  private def formatContactsList(response: ContactsListResponse): String = {
    val contacts = response.contacts.map { c =>
      DLCContactDb(
        alias = c.alias,
        address = NetworkUtil
          .parseInetSocketAddress(c.address,
                                  org.bitcoins.core.config.DLC.DefaultPort),
        memo = c.memo
      )
    }

    val json = ujson.Arr.from(
      contacts.map(c =>
        upickle.default.writeJs(c)(using Picklers.contactDbPickler))
    )

    jsValueToString(json)
  }
}
