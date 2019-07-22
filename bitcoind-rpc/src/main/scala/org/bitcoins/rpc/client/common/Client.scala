package org.bitcoins.rpc.client.common

import java.util.UUID

import akka.actor.ActorSystem
import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.{ActorMaterializer, StreamTcpException}
import akka.util.ByteString
import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.serializers.JsonSerializers._
import org.bitcoins.rpc.util.AsyncUtil
import play.api.libs.json._

import scala.concurrent._
import scala.concurrent.duration.DurationInt
import scala.sys.process._
import scala.util.{Failure, Success, Try}
import java.nio.file.Files

import org.bitcoins.rpc.config.BitcoindAuthCredentials.CookieBased
import org.bitcoins.rpc.config.BitcoindAuthCredentials.PasswordBased
import java.nio.file.Path

import org.bitcoins.rpc.config.BitcoindAuthCredentials
import org.bitcoins.rpc.BitcoindException

/**
  * This is the base trait for Bitcoin Core
  * RPC clients. It defines no RPC calls
  * except for the a ping. It contains functionality
  * and utilities useful when working with an RPC
  * client, like data directories, log files
  * and whether or not the client is started.
  */
trait Client extends BitcoinSLogger {
  def version: BitcoindVersion
  protected val instance: BitcoindInstance

  /**
    * The log file of the Bitcoin Core daemon
    */
  lazy val logFile: Path = {

    val prefix = instance.network match {
      case MainNet  => ""
      case TestNet3 => "testnet"
      case RegTest  => "regtest"
    }
    instance.datadir.toPath.resolve(prefix).resolve("debug.log")
  }

  /** The configuration file of the Bitcoin Core daemon */
  lazy val confFile: Path =
    instance.datadir.toPath.resolve("bitcoin.conf")

  implicit protected val system: ActorSystem
  implicit protected val materializer: ActorMaterializer =
    ActorMaterializer.create(system)
  implicit protected val executor: ExecutionContext = system.getDispatcher
  implicit protected val network: NetworkParameters = instance.network

  /**
    * This is here (and not in JsonWrriters)
    * so that the implicit network val is accessible
    */
  implicit object ECPrivateKeyWrites extends Writes[ECPrivateKey] {
    override def writes(o: ECPrivateKey): JsValue = JsString(o.toWIF(network))
  }

  implicit val eCPrivateKeyWrites: Writes[ECPrivateKey] = ECPrivateKeyWrites
  implicit val importMultiAddressWrites: Writes[RpcOpts.ImportMultiAddress] =
    Json.writes[RpcOpts.ImportMultiAddress]
  implicit val importMultiRequestWrites: Writes[RpcOpts.ImportMultiRequest] =
    Json.writes[RpcOpts.ImportMultiRequest]
  private val resultKey: String = "result"
  private val errorKey: String = "error"

  def getDaemon: BitcoindInstance = instance

  /** Starts bitcoind on the local system.
    * @return a future that completes when bitcoind is fully started.
    *         This future times out after 60 seconds if the client
    *         cannot be started
    */
  def start(): Future[Unit] = {
    if (version != BitcoindVersion.Unknown) {
      val foundVersion = instance.getVersion
      if (foundVersion != version) {
        throw new RuntimeException(
          s"Wrong version for bitcoind RPC client! Expected $version, got $foundVersion")
      }
    }

    val binaryPath = instance.binary.getAbsolutePath
    val cmd = List(binaryPath,
                   "-datadir=" + instance.datadir,
                   "-rpcport=" + instance.rpcUri.getPort,
                   "-port=" + instance.uri.getPort)

    logger.debug(
      s"starting bitcoind with datadir ${instance.datadir} and binary path $binaryPath")
    val _ = Process(cmd).run()

    def isStartedF: Future[Boolean] = {
      val started: Promise[Boolean] = Promise()

      val pingF = bitcoindCall[Unit]("ping", printError = false)
      pingF.onComplete {
        case Success(_) => started.success(true)
        case Failure(_) => started.success(false)
      }

      started.future
    }

    // if we're doing cookie based authentication, we might attempt
    // to read the cookie file before it's written. this ensures
    // we avoid that
    val awaitCookie: BitcoindAuthCredentials => Future[Unit] = {
      case cookie: CookieBased =>
        val cookieExistsF =
          AsyncUtil.retryUntilSatisfied(Files.exists(cookie.cookiePath))
        cookieExistsF.onComplete {
          case Failure(exception) =>
            logger.error(s"Cookie filed was never created! $exception")
          case _: Success[_] =>
        }
        cookieExistsF
      case _: PasswordBased => Future.successful(())

    }

    val started = {
      for {
        _ <- awaitCookie(instance.authCredentials)
        _ <- AsyncUtil.retryUntilSatisfiedF(() => isStartedF,
                                            duration = 1.seconds,
                                            maxTries = 60)
      } yield ()
    }

    started.onComplete {
      case Success(_) => logger.debug(s"started bitcoind")
      case Failure(exc) =>
        logger.info(
          s"Could not start bitcoind instance! Message: ${exc.getMessage}")
        // When we're unable to start bitcoind that's most likely
        // either a configuration error or bug in Bitcoin-S. In either
        // case it's much easier to debug this with conf and logs
        // dumped somewhere. Especially in tests this is
        // convenient, as our test framework deletes the data directories
        // of our instances. We don't want to do this on mainnet,
        // as both the logs and conf file most likely contain sensitive
        // information
        if (network != MainNet) {
          val tempfile = Files.createTempFile("bitcoind-log-", ".dump")
          val logfile = Files.readAllBytes(logFile)
          Files.write(tempfile, logfile)
          logger.info(s"Dumped debug.log to $tempfile")

          val otherTempfile = Files.createTempFile("bitcoin-conf-", ".dump")
          val conffile = Files.readAllBytes(confFile)
          Files.write(otherTempfile, conffile)
          logger.info(s"Dumped bitcoin.conf to $otherTempfile")
        }
    }

    started
  }

  /**
    * Checks whether the underlying bitcoind daemon is running
    */
  def isStartedF: Future[Boolean] = {
    def tryPing: Future[Boolean] = {
      val request = buildRequest(instance, "ping", JsArray.empty)
      val responseF = sendRequest(request)

      val payloadF: Future[JsValue] =
        responseF.flatMap(getPayload(_, command = "ping", request = request))

      // Ping successful if no error can be parsed from the payload
      val parsedF = payloadF.map { payload =>
        (payload \ errorKey).validate[BitcoindException] match {
          case _: JsSuccess[BitcoindException] => false
          case _: JsError                      => true
        }
      }

      parsedF.recover {
        case exc: StreamTcpException
            if exc.getMessage.contains("Connection refused") =>
          false
      }
    }

    instance.authCredentials match {
      case cookie: CookieBased if Files.notExists(cookie.cookiePath) =>
        // if the cookie file doesn't exist we're not started
        Future.successful(false)
      case (CookieBased(_, _) | PasswordBased(_, _)) => tryPing
    }
  }

  /**
    * Checks whether the underlyind bitcoind daemon is stopped
    */
  def isStoppedF: Future[Boolean] = {
    isStartedF.map(started => !started)
  }

  // This RPC call is here to avoid circular trait depedency
  def ping(): Future[Unit] = {
    bitcoindCall[Unit]("ping")
  }

  protected def bitcoindCall[T](
      command: String,
      parameters: List[JsValue] = List.empty,
      printError: Boolean = true)(
      implicit
      reader: Reads[T]): Future[T] = {

    val request = buildRequest(instance, command, JsArray(parameters))
    val responseF = sendRequest(request)

    val payloadF: Future[JsValue] =
      responseF.flatMap(getPayload(_, command, request, parameters))

    payloadF.map { payload =>
      /**
        * These lines are handy if you want to inspect what's being sent to and
        * returned from bitcoind before it's parsed into a Scala type. However,
        * there will sensitive material in some of those calls (private keys,
        * XPUBs, balances, etc). It's therefore not a good idea to enable
        * this logging in production.
        */
      // logger.info(
      // s"Command: $command ${parameters.map(_.toString).mkString(" ")}")
      // logger.info(s"Payload: \n${Json.prettyPrint(payload)}")
      parseResult(result = (payload \ resultKey).validate[T],
                  json = payload,
                  printError = printError,
                  command = command)
    }
  }

  protected def buildRequest(
      instance: BitcoindInstance,
      methodName: String,
      params: JsArray): HttpRequest = {
    val uuid = UUID.randomUUID().toString

    val m: Map[String, JsValue] = Map("method" -> JsString(methodName),
                                      "params" -> params,
                                      "id" -> JsString(uuid))

    val jsObject = JsObject(m)

    // Would toString work?
    val uri = "http://" + instance.rpcUri.getHost + ":" + instance.rpcUri.getPort
    val username = instance.authCredentials.username
    val password = instance.authCredentials.password
    HttpRequest(
      method = HttpMethods.POST,
      uri,
      entity = HttpEntity(ContentTypes.`application/json`, jsObject.toString()))
      .addCredentials(
        HttpCredentials.createBasicHttpCredentials(username, password))
  }

  protected def sendRequest(req: HttpRequest): Future[HttpResponse] = {
    Http(materializer.system).singleRequest(req)
  }

  /** Parses the payload of the given response into JSON.
    *
    * The command, parameters and request are given as debug parameters,
    * and only used for printing diagnostics if things go belly-up.
    */
  protected def getPayload(
      response: HttpResponse,
      command: String,
      request: HttpRequest,
      parameters: List[JsValue] = List.empty): Future[JsValue] = {
    val payloadF = response.entity.dataBytes.runFold(ByteString.empty)(_ ++ _)

    payloadF.flatMap { payload =>
      Try(Json.parse(payload.decodeString(ByteString.UTF_8))) match {
        case Failure(err) =>
          if (network != MainNet) {
            logger.error(s"Error when parsing result of command: $command")
            logger.error(s"Parameters: ${Json.stringify(JsArray(parameters))}")
            logger.error(s"Sent HTTP request: $request")
            logger.error(s"Received HTTP response: $response")
            logger.error(s"Error: $err")

          }
          Future.failed(err)
        case Success(js) => Future.successful(js)
      }
    }
  }

  // Should both logging and throwing be happening?
  private def parseResult[T](
      result: JsResult[T],
      json: JsValue,
      printError: Boolean,
      command: String
  ): T = {
    checkUnitError[T](result, json, printError)

    result match {
      case JsSuccess(value, _) => value
      case res: JsError =>
        (json \ errorKey).validate[BitcoindException] match {
          case JsSuccess(err, _) =>
            if (printError) {
              logger.error(s"$err")
            }
            throw err
          case _: JsError =>
            val jsonResult = (json \ resultKey).get
            val errString =
              s"Error when parsing result of '$command': ${JsError.toJson(res).toString}!"
            if (printError) logger.error(errString + s"JSON: $jsonResult")
            throw new IllegalArgumentException(
              s"Could not parse JsResult: $jsonResult! Error: $errString")
        }
    }
  }

  // Catches errors thrown by calls with Unit as the expected return type (which isn't handled by UnitReads)
  private def checkUnitError[T](
      result: JsResult[T],
      json: JsValue,
      printError: Boolean): Unit = {
    if (result == JsSuccess(())) {
      (json \ errorKey).validate[BitcoindException] match {
        case JsSuccess(err, _) =>
          if (printError) {
            logger.error(s"$err")
          }
          throw err
        case _: JsError =>
      }
    }
  }

}
