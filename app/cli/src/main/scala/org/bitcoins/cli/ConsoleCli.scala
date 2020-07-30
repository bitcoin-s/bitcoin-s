package org.bitcoins.cli

import org.bitcoins.cli.CliCommand._
import org.bitcoins.cli.CliReaders._
import org.bitcoins.commons.jsonmodels.wallet.CoinSelectionAlgo
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.sbclient.{Exchange, TradingPair}
import org.bitcoins.commons.serializers.Picklers._
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.{
  EmptyTransaction,
  Transaction,
  TransactionOutPoint
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}
import scopt.OParser
import ujson.{Num, Str}
import upickle.{default => up}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object ConsoleCli {

  def parser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]

    import builder._
    OParser.sequence(
      programName("bitcoin-s-cli"),
      opt[NetworkParameters]('n', "network")
        .action((np, conf) => conf.copy(network = Some(np)))
        .text("Select the active network."),
      opt[Unit]("debug")
        .action((_, conf) => conf.copy(debug = true))
        .text("Print debugging information"),
      opt[Int]("rpcport")
        .action((port, conf) => conf.copy(rpcPort = port))
        .text(s"The port to send our rpc request to on the server"),
      help('h', "help").text("Display this help message and exit"),
      note(sys.props("line.separator") + "Commands:"),
      note(sys.props("line.separator") + "===Blockchain ==="),
      cmd("getblockcount")
        .action((_, conf) => conf.copy(command = GetBlockCount))
        .text(s"Get the block height"),
      cmd("getfiltercount")
        .action((_, conf) => conf.copy(command = GetFilterCount))
        .text(s"Get the number of filters"),
      cmd("getfilterheadercount")
        .action((_, conf) => conf.copy(command = GetFilterHeaderCount))
        .text(s"Get the number of filter headers"),
      cmd("getbestblockhash")
        .action((_, conf) => conf.copy(command = GetBestBlockHash))
        .text(s"Get the best block hash"),
      cmd("decoderawtransaction")
        .hidden()
        .action((_, conf) =>
          conf.copy(command = DecodeRawTransaction(EmptyTransaction)))
        .text(s"Decode the given raw hex transaction")
        .children(opt[Transaction]("tx")
          .required()
          .action((tx, conf) =>
            conf.copy(command = conf.command match {
              case decode: DecodeRawTransaction => decode.copy(transaction = tx)
              case other                        => other
            }))),
      note(sys.props("line.separator") + "=== Wallet ==="),
      cmd("rescan")
        .action((_, conf) =>
          conf.copy(
            command = Rescan(addressBatchSize = Option.empty,
                             startBlock = Option.empty,
                             endBlock = Option.empty,
                             force = false,
                             ignoreCreationTime = false)))
        .text(s"Rescan for wallet UTXOs")
        .children(
          opt[Unit]("force")
            .text("Clears existing wallet records. Warning! Use with caution!")
            .optional()
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  rescan.copy(force = true)
                case other => other
              })),
          opt[Int]("batch-size")
            .text("Number of filters that can be matched in one batch")
            .optional()
            .action((batchSize, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  rescan.copy(addressBatchSize = Option(batchSize))
                case other => other
              })),
          opt[BlockStamp]("start")
            .text("Start height")
            .optional()
            .action((start, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  // Need to ignoreCreationTime so we try to call
                  // rescan with rescanNeutrinoWallet with a block
                  // and a creation time
                  rescan.copy(startBlock = Option(start),
                              ignoreCreationTime = true)
                case other => other
              })),
          opt[BlockStamp]("end")
            .text("End height")
            .optional()
            .action((end, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  rescan.copy(endBlock = Option(end))
                case other => other
              })),
          opt[Unit]("ignorecreationtime")
            .text("Ignores the wallet creation date and will instead do a full rescan")
            .optional()
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  rescan.copy(ignoreCreationTime = true)
                case other => other
              }))
        ),
      cmd("isempty")
        .action((_, conf) => conf.copy(command = IsEmpty))
        .text("Checks if the wallet contains any data"),
      cmd("createdlcoffer")
        .hidden()
        .action((_, conf) =>
          conf.copy(
            command = CreateDLCOffer(OracleInfo.dummy,
                                     ContractInfo.empty,
                                     Satoshis.zero,
                                     None,
                                     UInt32.zero,
                                     UInt32.zero,
                                     escaped = false)))
        .text("Creates a DLC offer that another party can accept")
        .children(
          opt[OracleInfo]("oracleInfo")
            .required()
            .action((info, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(oracleInfo = info)
                case other => other
              })),
          opt[ContractInfo]("contractInfo")
            .required()
            .action((info, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(contractInfo = info)
                case other => other
              })),
          opt[Satoshis]("collateral")
            .required()
            .action((collateral, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(collateral = collateral)
                case other => other
              })),
          opt[SatoshisPerVirtualByte]("feerate")
            .optional()
            .action((feeRate, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(feeRateOpt = Some(feeRate))
                case other => other
              })),
          opt[UInt32]("locktime")
            .required()
            .action((locktime, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(locktime = locktime)
                case other => other
              })),
          opt[UInt32]("refundlocktime")
            .required()
            .action((refundLT, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(refundLT = refundLT)
                case other => other
              })),
          opt[Unit]("escaped")
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case create: CreateDLCOffer =>
                  create.copy(escaped = true)
                case other => other
              }))
        ),
      cmd("acceptdlcoffer")
        .hidden()
        .action((_, conf) =>
          conf.copy(command = AcceptDLCOffer(null, escaped = false)))
        .text("Accepts a DLC offer given from another party")
        .children(
          opt[DLCOffer]("offer").required
            .action((offer, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOffer =>
                  accept.copy(offer = offer)
                case other => other
              })),
          opt[Unit]("escaped")
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOffer =>
                  accept.copy(escaped = true)
                case other => other
              }))
        ),
      cmd("signdlc")
        .hidden()
        .action((_, conf) =>
          conf.copy(command = SignDLC(null, escaped = false)))
        .text("Signs a DLC")
        .children(
          opt[DLCAccept]("accept").required
            .action((accept, conf) =>
              conf.copy(command = conf.command match {
                case signDLC: SignDLC =>
                  signDLC.copy(accept = accept)
                case other => other
              })),
          opt[Unit]("escaped")
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case signDLC: SignDLC =>
                  signDLC.copy(escaped = true)
                case other => other
              }))
        ),
      cmd("adddlcsigs")
        .hidden()
        .action((_, conf) => conf.copy(command = AddDLCSigs(null)))
        .text("Adds DLC Signatures into the database")
        .children(
          opt[DLCSign]("sigs").required
            .action((sigs, conf) =>
              conf.copy(command = conf.command match {
                case addDLCSigs: AddDLCSigs =>
                  addDLCSigs.copy(sigs = sigs)
                case other => other
              }))
        ),
      cmd("initdlcmutualclose")
        .hidden()
        .action((_, conf) =>
          conf.copy(command = InitDLCMutualClose(null, null, escaped = false)))
        .text("Sign Mutual Close Tx for given oracle event")
        .children(
          opt[Sha256DigestBE]("eventid").required
            .action((eventId, conf) =>
              conf.copy(command = conf.command match {
                case initClose: InitDLCMutualClose =>
                  initClose.copy(eventId = eventId)
                case other => other
              })),
          opt[SchnorrDigitalSignature]("oraclesig").required
            .action((sig, conf) =>
              conf.copy(command = conf.command match {
                case initClose: InitDLCMutualClose =>
                  initClose.copy(oracleSig = sig)
                case other => other
              })),
          opt[Unit]("escaped")
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case initClose: InitDLCMutualClose =>
                  initClose.copy(escaped = true)
                case other => other
              }))
        ),
      cmd("acceptdlcmutualclose")
        .hidden()
        .action((_, conf) =>
          conf.copy(command = AcceptDLCMutualClose(null, noBroadcast = false)))
        .text("Sign Mutual Close Tx for given oracle event")
        .children(
          opt[DLCMutualCloseSig]("closesig").required
            .action((closeSig, conf) =>
              conf.copy(command = conf.command match {
                case acceptClose: AcceptDLCMutualClose =>
                  acceptClose.copy(mutualCloseSig = closeSig)
                case other => other
              })),
          opt[Unit]("noBroadcast").optional
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case acceptClose: AcceptDLCMutualClose =>
                  acceptClose.copy(noBroadcast = true)
                case other => other
              }))
        ),
      cmd("getdlcfundingtx")
        .hidden()
        .action((_, conf) => conf.copy(command = GetDLCFundingTx(null)))
        .text("Returns the Funding Tx corresponding to the DLC with the given eventId")
        .children(
          opt[Sha256DigestBE]("eventid").required
            .action((eventId, conf) =>
              conf.copy(command = conf.command match {
                case getDLCFundingTx: GetDLCFundingTx =>
                  getDLCFundingTx.copy(eventId = eventId)
                case other => other
              }))
        ),
      cmd("broadcastdlcfundingtx")
        .hidden()
        .action((_, conf) => conf.copy(command = BroadcastDLCFundingTx(null)))
        .text("Broadcasts the funding Tx corresponding to the DLC with the given eventId")
        .children(
          opt[Sha256DigestBE]("eventid").required
            .action((eventId, conf) =>
              conf.copy(command = conf.command match {
                case broadcastDLCFundingTx: BroadcastDLCFundingTx =>
                  broadcastDLCFundingTx.copy(eventId = eventId)
                case other => other
              }))
        ),
      cmd("executedlcunilateralclose")
        .hidden()
        .action((_, conf) =>
          conf.copy(command =
            ExecuteDLCUnilateralClose(null, null, noBroadcast = false)))
        .text("Executes a unilateral close for the DLC with the given eventId")
        .children(
          opt[Sha256DigestBE]("eventid").required
            .action((eventId, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCUnilateralClose: ExecuteDLCUnilateralClose =>
                  executeDLCUnilateralClose.copy(eventId = eventId)
                case other => other
              })),
          opt[SchnorrDigitalSignature]("oraclesig").required
            .action((sig, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCUnilateralClose: ExecuteDLCUnilateralClose =>
                  executeDLCUnilateralClose.copy(oracleSig = sig)
                case other => other
              })),
          opt[Unit]("noBroadcast").optional
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCUnilateralClose: ExecuteDLCUnilateralClose =>
                  executeDLCUnilateralClose.copy(noBroadcast = true)
                case other => other
              }))
        ),
      cmd("executedlcremoteunilateralclose")
        .hidden()
        .action((_, conf) =>
          conf.copy(
            command = ExecuteDLCRemoteUnilateralClose(null,
                                                      EmptyTransaction,
                                                      noBroadcast = false)))
        .text("Executes a unilateral close for the DLC with the given eventId")
        .children(
          opt[Sha256DigestBE]("eventid").required
            .action((eventId, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCRemoteUnilateralClose: ExecuteDLCRemoteUnilateralClose =>
                  executeDLCRemoteUnilateralClose.copy(eventId = eventId)
                case other => other
              })),
          opt[Transaction]("forceCloseTx").required
            .action((cet, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCRemoteUnilateralClose: ExecuteDLCRemoteUnilateralClose =>
                  executeDLCRemoteUnilateralClose.copy(cet = cet)
                case other => other
              })),
          opt[Unit]("noBroadcast").optional
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCRemoteUnilateralClose: ExecuteDLCRemoteUnilateralClose =>
                  executeDLCRemoteUnilateralClose.copy(noBroadcast = true)
                case other => other
              }))
        ),
      cmd("executedlcforceclose")
        .hidden()
        .action((_, conf) =>
          conf.copy(
            command = ExecuteDLCForceClose(null, null, noBroadcast = false)))
        .text("Executes a force close for the DLC with the given eventId")
        .children(
          opt[Sha256DigestBE]("eventid").required
            .action((eventId, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCForceClose: ExecuteDLCForceClose =>
                  executeDLCForceClose.copy(eventId = eventId)
                case other => other
              })),
          opt[SchnorrDigitalSignature]("oraclesig").required
            .action((sig, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCForceClose: ExecuteDLCForceClose =>
                  executeDLCForceClose.copy(oracleSig = sig)
                case other => other
              })),
          opt[Unit]("noBroadcast").optional
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCForceClose: ExecuteDLCForceClose =>
                  executeDLCForceClose.copy(noBroadcast = true)
                case other => other
              }))
        ),
      cmd("claimdlcremotefunds")
        .hidden()
        .action((_, conf) =>
          conf.copy(command =
            ClaimDLCRemoteFunds(null, EmptyTransaction, noBroadcast = false)))
        .text("Claims the remote funds for the corresponding DLC")
        .children(
          opt[Sha256DigestBE]("eventid").required
            .action((eventId, conf) =>
              conf.copy(command = conf.command match {
                case claimDLCRemoteFunds: ClaimDLCRemoteFunds =>
                  claimDLCRemoteFunds.copy(eventId = eventId)
                case other => other
              })),
          opt[Transaction]("forceclosetx")
            .required()
            .action((tx, conf) =>
              conf.copy(command = conf.command match {
                case claimDLCRemoteFunds: ClaimDLCRemoteFunds =>
                  claimDLCRemoteFunds.copy(forceCloseTx = tx)
                case other => other
              })),
          opt[Unit]("noBroadcast")
            .optional()
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case claimDLCRemoteFunds: ClaimDLCRemoteFunds =>
                  claimDLCRemoteFunds.copy(noBroadcast = true)
                case other => other
              }))
        ),
      cmd("executedlcrefund")
        .hidden()
        .action((_, conf) =>
          conf.copy(command = ExecuteDLCRefund(null, noBroadcast = false)))
        .text("Executes the Refund transaction for the given DLC")
        .children(
          opt[Sha256DigestBE]("eventid").required
            .action((eventId, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCRefund: ExecuteDLCRefund =>
                  executeDLCRefund.copy(eventId = eventId)
                case other => other
              })),
          opt[Unit]("noBroadcast").optional
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCRefund: ExecuteDLCRefund =>
                  executeDLCRefund.copy(noBroadcast = true)
                case other => other
              }))
        ),
      cmd("claimdlcpenaltyfunds")
        .hidden()
        .action((_, conf) =>
          conf.copy(command =
            ClaimDLCPenaltyFunds(null, EmptyTransaction, noBroadcast = false)))
        .text("Claims the penalty funds for the corresponding DLC")
        .children(
          opt[Sha256DigestBE]("eventid").required
            .action((eventId, conf) =>
              conf.copy(command = conf.command match {
                case claimDLCPenaltyFunds: ClaimDLCPenaltyFunds =>
                  claimDLCPenaltyFunds.copy(eventId = eventId)
                case other => other
              })),
          opt[Transaction]("forceclosetx")
            .required()
            .action((tx, conf) =>
              conf.copy(command = conf.command match {
                case claimDLCPenaltyFunds: ClaimDLCPenaltyFunds =>
                  claimDLCPenaltyFunds.copy(forceCloseTx = tx)
                case other => other
              })),
          opt[Unit]("noBroadcast")
            .optional()
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case claimDLCPenaltyFunds: ClaimDLCPenaltyFunds =>
                  claimDLCPenaltyFunds.copy(noBroadcast = true)
                case other => other
              }))
        ),
      cmd("opensbchannel")
        .action((_, conf) => conf.copy(command = OpenSbChannel(0.satoshis)))
        .text("Opens a channel to the suredbits node")
        .children(
          arg[Satoshis]("amount").required
            .action((sats, conf) =>
              conf.copy(command = conf.command match {
                case openSbChannel: OpenSbChannel =>
                  openSbChannel.copy(amount = sats)
                case other => other
              }))
        ),
      cmd("getsbpubkey")
        .action((_, conf) => conf.copy(command = GetSbPubKey(null, null)))
        .text("Gets the Suredbits public key for the given exchange pair")
        .children(
          arg[Exchange]("exchange").required
            .action((exchange, conf) =>
              conf.copy(command = conf.command match {
                case getSbPubKey: GetSbPubKey =>
                  getSbPubKey.copy(exchange = exchange)
                case other => other
              })),
          arg[TradingPair]("tradingpair").required
            .action((tradingPair, conf) =>
              conf.copy(command = conf.command match {
                case getSbPubKey: GetSbPubKey =>
                  getSbPubKey.copy(tradingPair = tradingPair)
                case other => other
              }))
        ),
      cmd("getsbrvalue")
        .action((_, conf) => conf.copy(command = GetSbRValue(null, null)))
        .text("Gets the Suredbits r value for the given exchange pair")
        .children(
          arg[Exchange]("exchange").required
            .action((exchange, conf) =>
              conf.copy(command = conf.command match {
                case getSbRValue: GetSbRValue =>
                  getSbRValue.copy(exchange = exchange)
                case other => other
              })),
          arg[TradingPair]("tradingpair").required
            .action((tradingPair, conf) =>
              conf.copy(command = conf.command match {
                case getSbRValue: GetSbRValue =>
                  getSbRValue.copy(tradingPair = tradingPair)
                case other => other
              }))
        ),
      cmd("getsboracleinfo")
        .action((_, conf) => conf.copy(command = GetSbOracleInfo(null, null)))
        .text(
          "Gets the Suredbits pubkey and r value for the given exchange pair")
        .children(
          arg[Exchange]("exchange").required
            .action((exchange, conf) =>
              conf.copy(command = conf.command match {
                case getSbOracleInfo: GetSbOracleInfo =>
                  getSbOracleInfo.copy(exchange = exchange)
                case other => other
              })),
          arg[TradingPair]("tradingpair").required
            .action((tradingPair, conf) =>
              conf.copy(command = conf.command match {
                case getSbOracleInfo: GetSbOracleInfo =>
                  getSbOracleInfo.copy(tradingPair = tradingPair)
                case other => other
              }))
        ),
      cmd("getsblastsig")
        .action((_, conf) => conf.copy(command = GetSbLastSig(null, null)))
        .text("Gets the Suredbits signature for the given exchange pair")
        .children(
          arg[Exchange]("exchange").required
            .action((exchange, conf) =>
              conf.copy(command = conf.command match {
                case getSbLastSig: GetSbLastSig =>
                  getSbLastSig.copy(exchange = exchange)
                case other => other
              })),
          arg[TradingPair]("tradingpair").required
            .action((tradingPair, conf) =>
              conf.copy(command = conf.command match {
                case getSbLastSig: GetSbLastSig =>
                  getSbLastSig.copy(tradingPair = tradingPair)
                case other => other
              }))
        ),
      cmd("getbalance")
        .action((_, conf) => conf.copy(command = GetBalance(false)))
        .text("Get the wallet balance")
        .children(
          opt[Unit]("sats")
            .optional()
            .text("Display balance in satoshis")
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case getBalance: GetBalance =>
                  getBalance.copy(isSats = true)
                case other => other
              }))
        ),
      cmd("getconfirmedbalance")
        .action((_, conf) => conf.copy(command = GetConfirmedBalance(false)))
        .text("Get the wallet balance of confirmed utxos")
        .children(
          opt[Unit]("sats")
            .optional()
            .text("Display balance in satoshis")
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case getBalance: GetConfirmedBalance =>
                  getBalance.copy(isSats = true)
                case other => other
              }))
        ),
      cmd("getunconfirmedbalance")
        .action((_, conf) => conf.copy(command = GetUnconfirmedBalance(false)))
        .text("Get the wallet balance of unconfirmed utxos")
        .children(
          opt[Unit]("sats")
            .optional()
            .text("Display balance in satoshis")
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case getBalance: GetUnconfirmedBalance =>
                  getBalance.copy(isSats = true)
                case other => other
              }))
        ),
      cmd("getutxos")
        .action((_, conf) => conf.copy(command = GetUtxos))
        .text("Returns list of all wallet utxos"),
      cmd("getaddresses")
        .action((_, conf) => conf.copy(command = GetAddresses))
        .text("Returns list of all wallet addresses currently being watched"),
      cmd("getspentaddresses")
        .action((_, conf) => conf.copy(command = GetSpentAddresses))
        .text(
          "Returns list of all wallet addresses that have received funds and been spent"),
      cmd("getfundedaddresses")
        .action((_, conf) => conf.copy(command = GetFundedAddresses))
        .text("Returns list of all wallet addresses that are holding funds"),
      cmd("getunusedaddresses")
        .action((_, conf) => conf.copy(command = GetUnusedAddresses))
        .text("Returns list of all wallet addresses that have not been used"),
      cmd("getaccounts")
        .action((_, conf) => conf.copy(command = GetAccounts))
        .text("Returns list of all wallet accounts"),
      cmd("createnewaccount")
        .action((_, conf) => conf.copy(command = CreateNewAccount))
        .text("Creates a new wallet account"),
      cmd("getaddressinfo")
        .action((_, conf) => conf.copy(command = GetAddressInfo(null)))
        .text("Returns list of all wallet accounts")
        .children(
          arg[BitcoinAddress]("address")
            .text("Address to get information about")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case getAddressInfo: GetAddressInfo =>
                  getAddressInfo.copy(address = addr)
                case other => other
              }))
        ),
      cmd("getnewaddress")
        .action((_, conf) => conf.copy(command = GetNewAddress))
        .text("Get a new address"),
      cmd("sendtoaddress")
        .action(
          // TODO how to handle null here?
          (_, conf) =>
            conf.copy(command =
              SendToAddress(null, 0.bitcoin, None, noBroadcast = false)))
        .text("Send money to the given address")
        .children(
          arg[BitcoinAddress]("address")
            .text("Address to send to")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case send: SendToAddress =>
                  send.copy(destination = addr)
                case other => other
              })),
          arg[Bitcoins]("amount")
            .text("amount to send in BTC")
            .required()
            .action((btc, conf) =>
              conf.copy(command = conf.command match {
                case send: SendToAddress =>
                  send.copy(amount = btc)
                case other => other
              })),
          opt[SatoshisPerVirtualByte]("feerate")
            .text("Fee rate in sats per virtual byte")
            .optional()
            .action((feeRate, conf) =>
              conf.copy(command = conf.command match {
                case send: SendToAddress =>
                  send.copy(satoshisPerVirtualByte = Some(feeRate))
                case other => other
              })),
          opt[Unit]("noBroadcast").optional
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case send: SendToAddress =>
                  send.copy(noBroadcast = true)
                case other => other
              }))
        ),
      cmd("sendfromoutpoints")
        .action((_, conf) =>
          conf.copy(
            command = SendFromOutPoints(Vector.empty, null, 0.bitcoin, None)))
        .text("Send money to the given address")
        .children(
          arg[Seq[TransactionOutPoint]]("outpoints")
            .text("Out Points to send from")
            .required()
            .action((outPoints, conf) =>
              conf.copy(command = conf.command match {
                case send: SendFromOutPoints =>
                  send.copy(outPoints = outPoints.toVector)
                case other => other
              })),
          arg[BitcoinAddress]("address")
            .text("Address to send to")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case send: SendFromOutPoints =>
                  send.copy(destination = addr)
                case other => other
              })),
          arg[Bitcoins]("amount")
            .text("amount to send in BTC")
            .required()
            .action((btc, conf) =>
              conf.copy(command = conf.command match {
                case send: SendFromOutPoints =>
                  send.copy(amount = btc)
                case other => other
              })),
          opt[SatoshisPerVirtualByte]("feerate")
            .text("Fee rate in sats per virtual byte")
            .optional()
            .action((feeRate, conf) =>
              conf.copy(command = conf.command match {
                case send: SendFromOutPoints =>
                  send.copy(feeRateOpt = Some(feeRate))
                case other => other
              }))
        ),
      cmd("sendwithalgo")
        .action((_, conf) =>
          conf.copy(command = SendWithAlgo(null, 0.bitcoin, None, null)))
        .text(
          "Send money to the given address using a specific coin selection algo")
        .children(
          arg[BitcoinAddress]("address")
            .text("Address to send to")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case send: SendWithAlgo =>
                  send.copy(destination = addr)
                case other => other
              })),
          arg[Bitcoins]("amount")
            .text("amount to send in BTC")
            .required()
            .action((btc, conf) =>
              conf.copy(command = conf.command match {
                case send: SendWithAlgo =>
                  send.copy(amount = btc)
                case other => other
              })),
          arg[CoinSelectionAlgo]("algo")
            .text("Coin selection algo")
            .optional()
            .action((algo, conf) =>
              conf.copy(command = conf.command match {
                case send: SendWithAlgo =>
                  send.copy(algo = algo)
                case other => other
              })),
          opt[SatoshisPerVirtualByte]("feerate")
            .text("Fee rate in sats per virtual byte")
            .optional()
            .action((feeRate, conf) =>
              conf.copy(command = conf.command match {
                case send: SendWithAlgo =>
                  send.copy(feeRateOpt = Some(feeRate))
                case other => other
              }))
        ),
      cmd("opreturncommit")
        .action((_, conf) =>
          conf.copy(command = OpReturnCommit("", hashMessage = false, None)))
        .text("Creates OP_RETURN commitment transaction")
        .children(
          arg[String]("message")
            .text("message to put into OP_RETURN commitment")
            .required()
            .action((message, conf) =>
              conf.copy(command = conf.command match {
                case opReturnCommit: OpReturnCommit =>
                  opReturnCommit.copy(message = message)
                case other => other
              })),
          opt[Unit]("hashMessage")
            .text("should the message be hashed before commitment")
            .optional()
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case opReturnCommit: OpReturnCommit =>
                  opReturnCommit.copy(hashMessage = true)
                case other => other
              })),
          opt[SatoshisPerVirtualByte]("feerate")
            .text("Fee rate in sats per virtual byte")
            .optional()
            .action((feeRate, conf) =>
              conf.copy(command = conf.command match {
                case opReturnCommit: OpReturnCommit =>
                  opReturnCommit.copy(feeRateOpt = Some(feeRate))
                case other => other
              }))
        ),
      note(sys.props("line.separator") + "=== Network ==="),
      cmd("getpeers")
        .action((_, conf) => conf.copy(command = GetPeers))
        .text(s"List the connected peers"),
      cmd("stop")
        .action((_, conf) => conf.copy(command = Stop))
        .text("Request a graceful shutdown of Bitcoin-S"),
      cmd("sendrawtransaction")
        .action((_, conf) =>
          conf.copy(command = SendRawTransaction(EmptyTransaction)))
        .text("Broadcasts the raw transaction")
        .children(
          arg[Transaction]("tx")
            .text("Transaction serialized in hex")
            .required()
            .action((tx, conf) =>
              conf.copy(command = conf.command match {
                case sendRawTransaction: SendRawTransaction =>
                  sendRawTransaction.copy(tx = tx)
                case other => other
              }))
        ),
      note(sys.props("line.separator") + "=== PSBT ==="),
      cmd("combinepsbts")
        .action((_, conf) => conf.copy(command = CombinePSBTs(Seq.empty)))
        .text("Combines all the given PSBTs")
        .children(
          arg[Seq[PSBT]]("psbts")
            .text("PSBTs serialized in hex or base64 format")
            .required()
            .action((seq, conf) =>
              conf.copy(command = conf.command match {
                case combinePSBTs: CombinePSBTs =>
                  combinePSBTs.copy(psbts = seq)
                case other => other
              }))
        ),
      cmd("joinpsbts")
        .action((_, conf) => conf.copy(command = JoinPSBTs(Seq.empty)))
        .text("Combines all the given PSBTs")
        .children(
          arg[Seq[PSBT]]("psbts")
            .text("PSBTs serialized in hex or base64 format")
            .required()
            .action((seq, conf) =>
              conf.copy(command = conf.command match {
                case joinPSBTs: JoinPSBTs =>
                  joinPSBTs.copy(psbts = seq)
                case other => other
              }))
        ),
      cmd("finalizepsbt")
        .action((_, conf) => conf.copy(command = FinalizePSBT(PSBT.empty)))
        .text("Finalizes the given PSBT if it can")
        .children(
          arg[PSBT]("psbt")
            .text("PSBT serialized in hex or base64 format")
            .required()
            .action((psbt, conf) =>
              conf.copy(command = conf.command match {
                case finalizePSBT: FinalizePSBT =>
                  finalizePSBT.copy(psbt = psbt)
                case other => other
              }))
        ),
      cmd("extractfrompsbt")
        .action((_, conf) => conf.copy(command = ExtractFromPSBT(PSBT.empty)))
        .text("Extracts a transaction from the given PSBT if it can")
        .children(
          arg[PSBT]("psbt")
            .text("PSBT serialized in hex or base64 format")
            .required()
            .action((psbt, conf) =>
              conf.copy(command = conf.command match {
                case extractFromPSBT: ExtractFromPSBT =>
                  extractFromPSBT.copy(psbt = psbt)
                case other => other
              }))
        ),
      cmd("converttopsbt")
        .action((_, conf) =>
          conf.copy(command = ConvertToPSBT(EmptyTransaction)))
        .text("Creates an empty psbt from the given transaction")
        .children(
          arg[Transaction]("unsignedTx")
            .text("serialized unsigned transaction in hex")
            .required()
            .action((tx, conf) =>
              conf.copy(command = conf.command match {
                case convertToPSBT: ConvertToPSBT =>
                  convertToPSBT.copy(transaction = tx)
                case other => other
              }))
        ),
      checkConfig {
        case Config(NoCommand, _, _, _) =>
          failure("You need to provide a command!")
        case _ => success
      }
    )
  }

  def exec(args: String*): Try[String] = {
    val config = OParser.parse(parser, args.toVector, Config()) match {
      case None       => sys.exit(1)
      case Some(conf) => conf
    }

    exec(config.command, config)
  }

  def exec(command: CliCommand, config: Config): Try[String] = {
    import System.err.{println => printerr}

    /** Prints the given message to stderr if debug is set */
    def debug(message: Any): Unit = {
      if (config.debug) {
        printerr(s"DEBUG: $message")
      }
    }

    /** Prints the given message to stderr and exist */
    def error[T](message: String): Failure[T] = {
      Failure(new RuntimeException(message))
    }

    val requestParam: RequestParam = command match {
      case GetUtxos =>
        RequestParam("getutxos")
      case GetAddresses =>
        RequestParam("getaddresses")
      case GetSpentAddresses =>
        RequestParam("getspentaddresses")
      case GetFundedAddresses =>
        RequestParam("getfundedaddresses")
      case GetUnusedAddresses =>
        RequestParam("getunusedaddresses")
      case GetAccounts =>
        RequestParam("getaccounts")
      case CreateNewAccount =>
        RequestParam("createnewaccount")
      case IsEmpty =>
        RequestParam("isempty")
      // DLCs
      case CreateDLCOffer(oracleInfo,
                          contractInfo,
                          collateral,
                          feeRateOpt,
                          locktime,
                          refundLT,
                          escaped) =>
        RequestParam(
          "createdlcoffer",
          Seq(
            up.writeJs(oracleInfo),
            up.writeJs(contractInfo),
            up.writeJs(collateral),
            up.writeJs(feeRateOpt),
            up.writeJs(locktime),
            up.writeJs(refundLT),
            up.writeJs(escaped)
          )
        )
      case AcceptDLCOffer(offer, escaped) =>
        RequestParam("acceptdlcoffer",
                     Seq(up.writeJs(offer), up.writeJs(escaped)))
      case SignDLC(accept, escaped) =>
        RequestParam("signdlc", Seq(up.writeJs(accept), up.writeJs(escaped)))
      case AddDLCSigs(sigs) =>
        RequestParam("adddlcsigs", Seq(up.writeJs(sigs)))
      case InitDLCMutualClose(eventId, oracleSig, escaped) =>
        RequestParam(
          "initdlcmutualclose",
          Seq(up.writeJs(eventId), up.writeJs(oracleSig), up.writeJs(escaped)))
      case AcceptDLCMutualClose(mutualCloseSig, noBroadcast) =>
        RequestParam("acceptdlcmutualclose",
                     Seq(up.writeJs(mutualCloseSig), up.writeJs(noBroadcast)))
      case ExecuteDLCUnilateralClose(eventId, oracleSig, noBroadcast) =>
        RequestParam("executedlcunilateralclose",
                     Seq(up.writeJs(eventId),
                         up.writeJs(oracleSig),
                         up.writeJs(noBroadcast)))
      case ExecuteDLCRemoteUnilateralClose(eventId, cet, noBroadcast) =>
        RequestParam(
          "executedlcremoteunilateralclose",
          Seq(up.writeJs(eventId), up.writeJs(cet), up.writeJs(noBroadcast)))
      case GetDLCFundingTx(eventId) =>
        RequestParam("getdlcfundingtx", Seq(up.writeJs(eventId)))
      case BroadcastDLCFundingTx(eventId) =>
        RequestParam("broadcastdlcfundingtx", Seq(up.writeJs(eventId)))
      case ExecuteDLCForceClose(eventId, oracleSig, noBroadcast) =>
        RequestParam("executedlcforceclose",
                     Seq(up.writeJs(eventId),
                         up.writeJs(oracleSig),
                         up.writeJs(noBroadcast)))
      case ClaimDLCRemoteFunds(eventId, forceCloseTx, noBroadcast) =>
        RequestParam("claimdlcremotefunds",
                     Seq(up.writeJs(eventId),
                         up.writeJs(forceCloseTx),
                         up.writeJs(noBroadcast)))
      case ExecuteDLCRefund(eventId, noBroadcast) =>
        RequestParam("executedlcrefund",
                     Seq(up.writeJs(eventId), up.writeJs(noBroadcast)))
      case ClaimDLCPenaltyFunds(eventId, forceCloseTx, noBroadcast) =>
        RequestParam("claimdlcpenaltyfunds",
                     Seq(up.writeJs(eventId),
                         up.writeJs(forceCloseTx),
                         up.writeJs(noBroadcast)))
      // Suredbits Client
      case OpenSbChannel(amount) =>
        RequestParam("opensbchannel", Seq(up.writeJs(amount)))
      case GetSbPubKey(exchange, tradingPair) =>
        RequestParam("getsbpubkey",
                     Seq(up.writeJs(exchange), up.writeJs(tradingPair)))
      case GetSbRValue(exchange, tradingPair) =>
        RequestParam("getsbrvalue",
                     Seq(up.writeJs(exchange), up.writeJs(tradingPair)))
      case GetSbOracleInfo(exchange, tradingPair) =>
        RequestParam("getsboracleinfo",
                     Seq(up.writeJs(exchange), up.writeJs(tradingPair)))
      case GetSbLastSig(exchange, tradingPair) =>
        RequestParam("getsblastsig",
                     Seq(up.writeJs(exchange), up.writeJs(tradingPair)))
      // Wallet
      case GetBalance(isSats) =>
        RequestParam("getbalance", Seq(up.writeJs(isSats)))
      case GetConfirmedBalance(isSats) =>
        RequestParam("getconfirmedbalance", Seq(up.writeJs(isSats)))
      case GetUnconfirmedBalance(isSats) =>
        RequestParam("getunconfirmedbalance", Seq(up.writeJs(isSats)))
      case GetAddressInfo(address) =>
        RequestParam("getaddressinfo", Seq(up.writeJs(address)))
      case GetNewAddress =>
        RequestParam("getnewaddress")
      case Rescan(addressBatchSize,
                  startBlock,
                  endBlock,
                  force,
                  ignoreCreationTime) =>
        RequestParam("rescan",
                     Seq(up.writeJs(addressBatchSize),
                         up.writeJs(startBlock),
                         up.writeJs(endBlock),
                         up.writeJs(force),
                         up.writeJs(ignoreCreationTime)))

      case SendToAddress(address,
                         bitcoins,
                         satoshisPerVirtualByte,
                         noBroadcast) =>
        RequestParam("sendtoaddress",
                     Seq(up.writeJs(address),
                         up.writeJs(bitcoins),
                         up.writeJs(satoshisPerVirtualByte),
                         up.writeJs(noBroadcast)))
      case SendFromOutPoints(outPoints, address, bitcoins, feeRateOpt) =>
        RequestParam("sendfromoutpoints",
                     Seq(up.writeJs(outPoints),
                         up.writeJs(address),
                         up.writeJs(bitcoins),
                         up.writeJs(feeRateOpt)))
      case SendWithAlgo(address, bitcoins, feeRateOpt, algo) =>
        RequestParam("SendWithAlgo",
                     Seq(up.writeJs(address),
                         up.writeJs(bitcoins),
                         up.writeJs(feeRateOpt),
                         up.writeJs(algo)))
      case OpReturnCommit(message, hashMessage, satoshisPerVirtualByte) =>
        RequestParam("opreturncommit",
                     Seq(up.writeJs(message),
                         up.writeJs(hashMessage),
                         up.writeJs(satoshisPerVirtualByte)))
      // height
      case GetBlockCount => RequestParam("getblockcount")
      // filter count
      case GetFilterCount => RequestParam("getfiltercount")
      // filter header count
      case GetFilterHeaderCount => RequestParam("getfilterheadercount")
      // besthash
      case GetBestBlockHash => RequestParam("getbestblockhash")
      // peers
      case GetPeers => RequestParam("getpeers")
      case Stop     => RequestParam("stop")
      case SendRawTransaction(tx) =>
        RequestParam("sendrawtransaction", Seq(up.writeJs(tx)))
      // PSBTs
      case CombinePSBTs(psbts) =>
        RequestParam("combinepsbts", Seq(up.writeJs(psbts)))
      case JoinPSBTs(psbts) =>
        RequestParam("joinpsbts", Seq(up.writeJs(psbts)))
      case FinalizePSBT(psbt) =>
        RequestParam("finalizepsbt", Seq(up.writeJs(psbt)))
      case ExtractFromPSBT(psbt) =>
        RequestParam("extractfrompsbt", Seq(up.writeJs(psbt)))
      case ConvertToPSBT(tx) =>
        RequestParam("converttopsbt", Seq(up.writeJs(tx)))

      case DecodeRawTransaction(tx) =>
        RequestParam("decoderawtransaction", Seq(up.writeJs(tx)))

      case NoCommand => ???
    }

    Try {
      import com.softwaremill.sttp._
      implicit val backend: SttpBackend[Id, Nothing] =
        HttpURLConnectionBackend()
      val request =
        sttp
          .post(uri"http://$host:${config.rpcPort}/")
          .contentType("application/json")
          .body {
            val uuid = java.util.UUID.randomUUID.toString
            val paramsWithID: Map[String, ujson.Value] =
              requestParam.toJsonMap + ("id" -> up
                .writeJs(uuid))
            up.write(paramsWithID)
          }
      debug(s"HTTP request: $request")
      val response = request.send()

      debug(s"HTTP response:")
      debug(response)

      // in order to mimic Bitcoin Core we always send
      // an object looking like {"result": ..., "error": ...}
      val rawBody = response.body match {
        case Left(err)       => err
        case Right(response) => response
      }

      debug(s"Raw Message: $rawBody")
      val js = ujson.read(rawBody)
      val jsObjT =
        Try(js.obj).transform[mutable.LinkedHashMap[String, ujson.Value]](
          Success(_),
          _ => error(s"Response was not a JSON object! Got: $rawBody"))

      /** Gets the given key from jsObj if it exists
        * and is not null */
      def getKey(key: String): Option[ujson.Value] = {
        jsObjT.toOption.flatMap(_.get(key).flatMap(result =>
          if (result.isNull) None else Some(result)))
      }

      /** Converts a `ujson.Value` to String, making an
        * effort to avoid preceding and trailing `"`s */
      def jsValueToString(value: ujson.Value) =
        value match {
          case Str(string)             => string
          case Num(num) if num.isWhole => num.toLong.toString
          case Num(num)                => num.toString
          case rest: ujson.Value       => rest.toString()
        }

      (getKey("result"), getKey("error")) match {
        case (Some(result), None) =>
          Success(jsValueToString(result))
        case (None, Some(err)) =>
          val msg = jsValueToString(err)
          error(msg)
        case (None, None) | (Some(_), Some(_)) =>
          error(s"Got unexpected response: $rawBody")
      }
    }.flatten
  }

  def host = "localhost"

  case class RequestParam(
      method: String,
      params: Seq[ujson.Value.Value] = Nil) {

    lazy val toJsonMap: Map[String, ujson.Value] = {
      Map("method" -> method, "params" -> params)
    }
  }
}

case class Config(
    command: CliCommand = CliCommand.NoCommand,
    network: Option[NetworkParameters] = None,
    debug: Boolean = false,
    rpcPort: Int = 9999
)

object Config {
  val empty = Config()
}

sealed abstract class CliCommand

object CliCommand {
  case object NoCommand extends CliCommand

  sealed trait JsonResponse {
    def escaped: Boolean
  }

  sealed trait Broadcastable {
    def noBroadcast: Boolean
  }

  sealed trait PriceDataApiCall extends CliCommand {
    def exchange: Exchange
    def tradingPair: TradingPair
  }

  // DLC
  case class CreateDLCOffer(
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLT: UInt32,
      escaped: Boolean)
      extends CliCommand
      with JsonResponse

  case class AcceptDLCOffer(offer: DLCOffer, escaped: Boolean)
      extends CliCommand
      with JsonResponse

  case class SignDLC(accept: DLCAccept, escaped: Boolean)
      extends CliCommand
      with JsonResponse

  case class AddDLCSigs(sigs: DLCSign) extends CliCommand

  case class InitDLCMutualClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature,
      escaped: Boolean)
      extends CliCommand
      with JsonResponse

  case class AcceptDLCMutualClose(
      mutualCloseSig: DLCMutualCloseSig,
      noBroadcast: Boolean)
      extends CliCommand
      with Broadcastable

  case class GetDLCFundingTx(eventId: Sha256DigestBE) extends CliCommand

  case class BroadcastDLCFundingTx(eventId: Sha256DigestBE) extends CliCommand

  case class ExecuteDLCUnilateralClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature,
      noBroadcast: Boolean)
      extends CliCommand
      with Broadcastable

  case class ExecuteDLCRemoteUnilateralClose(
      eventId: Sha256DigestBE,
      cet: Transaction,
      noBroadcast: Boolean)
      extends CliCommand
      with Broadcastable

  case class ExecuteDLCForceClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature,
      noBroadcast: Boolean)
      extends CliCommand
      with Broadcastable

  case class ClaimDLCRemoteFunds(
      eventId: Sha256DigestBE,
      forceCloseTx: Transaction,
      noBroadcast: Boolean)
      extends CliCommand
      with Broadcastable

  case class ExecuteDLCRefund(eventId: Sha256DigestBE, noBroadcast: Boolean)
      extends CliCommand
      with Broadcastable

  case class ClaimDLCPenaltyFunds(
      eventId: Sha256DigestBE,
      forceCloseTx: Transaction,
      noBroadcast: Boolean)
      extends CliCommand
      with Broadcastable

  // Suredbits Client
  case class OpenSbChannel(amount: Satoshis) extends CliCommand

  case class GetSbPubKey(exchange: Exchange, tradingPair: TradingPair)
      extends PriceDataApiCall

  case class GetSbRValue(exchange: Exchange, tradingPair: TradingPair)
      extends PriceDataApiCall

  case class GetSbOracleInfo(exchange: Exchange, tradingPair: TradingPair)
      extends PriceDataApiCall

  case class GetSbLastSig(exchange: Exchange, tradingPair: TradingPair)
      extends PriceDataApiCall

  // Wallet
  case class SendToAddress(
      destination: BitcoinAddress,
      amount: Bitcoins,
      satoshisPerVirtualByte: Option[SatoshisPerVirtualByte],
      noBroadcast: Boolean)
      extends CliCommand
      with Broadcastable

  case class SendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      destination: BitcoinAddress,
      amount: Bitcoins,
      feeRateOpt: Option[SatoshisPerVirtualByte])
      extends CliCommand

  case class SendWithAlgo(
      destination: BitcoinAddress,
      amount: Bitcoins,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      algo: CoinSelectionAlgo)
      extends CliCommand

  case class OpReturnCommit(
      message: String,
      hashMessage: Boolean,
      feeRateOpt: Option[SatoshisPerVirtualByte])
      extends CliCommand
  case object GetNewAddress extends CliCommand
  case object GetUtxos extends CliCommand
  case object GetAddresses extends CliCommand
  case object GetSpentAddresses extends CliCommand
  case object GetFundedAddresses extends CliCommand
  case object GetUnusedAddresses extends CliCommand
  case object GetAccounts extends CliCommand
  case object CreateNewAccount extends CliCommand
  case object IsEmpty extends CliCommand
  case class GetBalance(isSats: Boolean) extends CliCommand
  case class GetConfirmedBalance(isSats: Boolean) extends CliCommand
  case class GetUnconfirmedBalance(isSats: Boolean) extends CliCommand
  case class GetAddressInfo(address: BitcoinAddress) extends CliCommand

  // Node
  case object GetPeers extends CliCommand
  case object Stop extends CliCommand
  case class SendRawTransaction(tx: Transaction) extends CliCommand

  // Chain
  case object GetBestBlockHash extends CliCommand
  case object GetBlockCount extends CliCommand
  case object GetFilterCount extends CliCommand
  case object GetFilterHeaderCount extends CliCommand
  case class DecodeRawTransaction(transaction: Transaction) extends CliCommand

  case class Rescan(
      addressBatchSize: Option[Int],
      startBlock: Option[BlockStamp],
      endBlock: Option[BlockStamp],
      force: Boolean,
      ignoreCreationTime: Boolean)
      extends CliCommand

  // PSBT
  case class CombinePSBTs(psbts: Seq[PSBT]) extends CliCommand
  case class JoinPSBTs(psbts: Seq[PSBT]) extends CliCommand
  case class FinalizePSBT(psbt: PSBT) extends CliCommand
  case class ExtractFromPSBT(psbt: PSBT) extends CliCommand
  case class ConvertToPSBT(transaction: Transaction) extends CliCommand
}
