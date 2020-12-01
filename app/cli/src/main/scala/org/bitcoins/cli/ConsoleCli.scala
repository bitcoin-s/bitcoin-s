package org.bitcoins.cli

import java.io.File
import java.nio.file.Path
import java.time.Instant

import org.bitcoins.cli.CliCommand._
import org.bitcoins.cli.CliReaders._
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.LockUnspentOutputParameter
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.serializers.Picklers._
import org.bitcoins.core.api.wallet.CoinSelectionAlgo
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{
  EmptyTransaction,
  Transaction,
  TransactionOutPoint
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.AddressLabelTag
import org.bitcoins.crypto.{
  SchnorrDigitalSignature,
  SchnorrNonce,
  Sha256DigestBE
}
import scodec.bits.ByteVector
import scopt.OParser
import ujson._
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
        .action((_, conf) =>
          conf.copy(command = DecodeRawTransaction(EmptyTransaction)))
        .text(s"Decode the given raw hex transaction")
        .children(arg[Transaction]("tx")
          .text("Transaction encoded in hex to decode")
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
        .action((_, conf) =>
          conf.copy(
            command = CreateDLCOffer(OracleInfo.dummy,
                                     ContractInfo.empty.toTLV,
                                     Satoshis.zero,
                                     None,
                                     UInt32.zero,
                                     UInt32.zero)))
        .text("Creates a DLC offer that another party can accept")
        .children(
          arg[OracleInfo]("oracleInfo")
            .required()
            .action((info, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(oracleInfo = info)
                case other => other
              })),
          arg[ContractInfoTLV]("contractInfo")
            .required()
            .action((info, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(contractInfo = info)
                case other => other
              })),
          arg[Satoshis]("collateral")
            .required()
            .action((collateral, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(collateral = collateral)
                case other => other
              })),
          arg[SatoshisPerVirtualByte]("feerate")
            .optional()
            .action((feeRate, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(feeRateOpt = Some(feeRate))
                case other => other
              })),
          arg[UInt32]("locktime")
            .required()
            .action((locktime, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(locktime = locktime)
                case other => other
              })),
          arg[UInt32]("refundlocktime")
            .required()
            .action((refundLT, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(refundLT = refundLT)
                case other => other
              }))
        ),
      cmd("acceptdlcoffer")
        .action((_, conf) => conf.copy(command = AcceptDLCOffer(null)))
        .text("Accepts a DLC offer given from another party")
        .children(
          arg[LnMessage[DLCOfferTLV]]("offer")
            .required()
            .action((offer, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOffer =>
                  accept.copy(offer = offer)
                case other => other
              }))
        ),
      cmd("acceptdlcofferfromfile")
        .action((_, conf) =>
          conf.copy(command = AcceptDLCOfferFromFile(new File("").toPath)))
        .text("Accepts a DLC offer given from another party")
        .children(
          arg[Path]("path")
            .required()
            .action((path, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOfferFromFile =>
                  accept.copy(path = path)
                case other => other
              }))
        ),
      cmd("signdlc")
        .action((_, conf) => conf.copy(command = SignDLC(null)))
        .text("Signs a DLC")
        .children(
          arg[LnMessage[DLCAcceptTLV]]("accept")
            .required()
            .action((accept, conf) =>
              conf.copy(command = conf.command match {
                case signDLC: SignDLC =>
                  signDLC.copy(accept = accept)
                case other => other
              }))
        ),
      cmd("signdlcfromfile")
        .action((_, conf) =>
          conf.copy(command = SignDLCFromFile(new File("").toPath)))
        .text("Signs a DLC")
        .children(
          arg[Path]("path")
            .required()
            .action((path, conf) =>
              conf.copy(command = conf.command match {
                case signDLC: SignDLCFromFile =>
                  signDLC.copy(path = path)
                case other => other
              }))
        ),
      cmd("adddlcsigs")
        .action((_, conf) => conf.copy(command = AddDLCSigs(null)))
        .text("Adds DLC Signatures into the database")
        .children(
          arg[LnMessage[DLCSignTLV]]("sigs")
            .required()
            .action((sigs, conf) =>
              conf.copy(command = conf.command match {
                case addDLCSigs: AddDLCSigs =>
                  addDLCSigs.copy(sigs = sigs)
                case other => other
              }))
        ),
      cmd("adddlcsigsfromfile")
        .action((_, conf) =>
          conf.copy(command = AddDLCSigsFromFile(new File("").toPath)))
        .text("Adds DLC Signatures into the database")
        .children(
          arg[Path]("path")
            .required()
            .action((path, conf) =>
              conf.copy(command = conf.command match {
                case addDLCSigs: AddDLCSigsFromFile =>
                  addDLCSigs.copy(path = path)
                case other => other
              }))
        ),
      cmd("getdlcfundingtx")
        .action((_, conf) => conf.copy(command = GetDLCFundingTx(null)))
        .text("Returns the Funding Tx corresponding to the DLC with the given contractId")
        .children(
          opt[ByteVector]("contractId")
            .required()
            .action((contractId, conf) =>
              conf.copy(command = conf.command match {
                case getDLCFundingTx: GetDLCFundingTx =>
                  getDLCFundingTx.copy(contractId = contractId)
                case other => other
              }))
        ),
      cmd("broadcastdlcfundingtx")
        .action((_, conf) => conf.copy(command = BroadcastDLCFundingTx(null)))
        .text("Broadcasts the funding Tx corresponding to the DLC with the given contractId")
        .children(
          arg[ByteVector]("contractId")
            .required()
            .action((contractId, conf) =>
              conf.copy(command = conf.command match {
                case broadcastDLCFundingTx: BroadcastDLCFundingTx =>
                  broadcastDLCFundingTx.copy(contractId = contractId)
                case other => other
              }))
        ),
      cmd("executedlc")
        .action((_, conf) =>
          conf.copy(command =
            ExecuteDLC(ByteVector.empty, Vector.empty, noBroadcast = false)))
        .text("Executes the DLC with the given contractId")
        .children(
          arg[ByteVector]("contractId")
            .required()
            .action((contractId, conf) =>
              conf.copy(command = conf.command match {
                case executeDLC: ExecuteDLC =>
                  executeDLC.copy(contractId = contractId)
                case other => other
              })),
          arg[Seq[SchnorrDigitalSignature]]("oraclesigs")
            .required()
            .action((sigs, conf) =>
              conf.copy(command = conf.command match {
                case executeDLC: ExecuteDLC =>
                  executeDLC.copy(oracleSigs = sigs.toVector)
                case other => other
              })),
          opt[Unit]("noBroadcast")
            .optional()
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case executeDLC: ExecuteDLC =>
                  executeDLC.copy(noBroadcast = true)
                case other => other
              }))
        ),
      cmd("executedlcrefund")
        .action((_, conf) =>
          conf.copy(command = ExecuteDLCRefund(null, noBroadcast = false)))
        .text("Executes the Refund transaction for the given DLC")
        .children(
          arg[ByteVector]("contractId")
            .required()
            .action((contractId, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCRefund: ExecuteDLCRefund =>
                  executeDLCRefund.copy(contractId = contractId)
                case other => other
              })),
          opt[Unit]("noBroadcast")
            .optional()
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCRefund: ExecuteDLCRefund =>
                  executeDLCRefund.copy(noBroadcast = true)
                case other => other
              }))
        ),
      cmd("getdlcs")
        .action((_, conf) => conf.copy(command = GetDLCs))
        .text("Returns all dlcs in the wallet"),
      cmd("getdlc")
        .action((_, conf) => conf.copy(command = GetDLC(Sha256DigestBE.empty)))
        .text("Gets a specific dlc in the wallet")
        .children(arg[Sha256DigestBE]("paramhash")
          .required()
          .action((paramHash, conf) =>
            conf.copy(command = conf.command match {
              case _: GetDLC => GetDLC(paramHash)
              case other     => other
            }))),
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
        .action((_, conf) => conf.copy(command = GetNewAddress(None)))
        .text("Get a new address")
        .children(
          opt[AddressLabelTag]("label")
            .text("The label name for the address to be linked to")
            .action((label, conf) =>
              conf.copy(command = conf.command match {
                case getNewAddress: GetNewAddress =>
                  getNewAddress.copy(labelOpt = Some(label))
                case other => other
              }))
        ),
      cmd("labeladdress")
        .action((_, conf) =>
          conf.copy(command = LabelAddress(null, AddressLabelTag(""))))
        .text("Add a label to the wallet address")
        .children(
          arg[BitcoinAddress]("address")
            .text("The address to label")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case labelAddress: LabelAddress =>
                  labelAddress.copy(address = addr)
                case other => other
              })),
          arg[AddressLabelTag]("label")
            .text("The label name for the address to be linked to")
            .required()
            .action((label, conf) =>
              conf.copy(command = conf.command match {
                case labelAddress: LabelAddress =>
                  labelAddress.copy(label = label)
                case other => other
              }))
        ),
      cmd("getaddresstags")
        .action((_, conf) => conf.copy(command = GetAddressTags(null)))
        .text("Get all the tags associated with this address")
        .children(
          arg[BitcoinAddress]("address")
            .text("The address to get with the associated tags")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case getAddressTags: GetAddressTags =>
                  getAddressTags.copy(address = addr)
                case other => other
              }))
        ),
      cmd("getaddresslabels")
        .action((_, conf) => conf.copy(command = GetAddressLabels(null)))
        .text("Get all the labels associated with this address")
        .children(
          arg[BitcoinAddress]("address")
            .text("The address to get with the associated labels")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case getAddressLabels: GetAddressLabels =>
                  getAddressLabels.copy(address = addr)
                case other => other
              }))
        ),
      cmd("dropaddresslabels")
        .action((_, conf) => conf.copy(command = DropAddressLabels(null)))
        .text("Drop all the labels associated with this address")
        .children(
          arg[BitcoinAddress]("address")
            .text("The address to drop the associated labels of")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case dropAddressLabels: DropAddressLabels =>
                  dropAddressLabels.copy(address = addr)
                case other => other
              }))
        ),
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
          opt[Unit]("noBroadcast")
            .optional()
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
      cmd("lockunspent")
        .action((_, conf) =>
          conf.copy(command = LockUnspent(unlock = false, Vector.empty)))
        .text("Temporarily lock (unlock=false) or unlock (unlock=true) specified transaction outputs." +
          "\nIf no transaction outputs are specified when unlocking then all current locked transaction outputs are unlocked.")
        .children(
          arg[Boolean]("unlock")
            .text("Whether to unlock (true) or lock (false) the specified transactions")
            .required()
            .action((unlock, conf) =>
              conf.copy(command = conf.command match {
                case lockUnspent: LockUnspent =>
                  lockUnspent.copy(unlock = unlock)
                case other => other
              })),
          arg[Seq[LockUnspentOutputParameter]]("transactions")
            .text("The transaction outpoints to unlock/lock")
            .required()
            .action((outPoints, conf) =>
              conf.copy(command = conf.command match {
                case lockUnspent: LockUnspent =>
                  lockUnspent.copy(outPoints = outPoints.toVector)
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
      note(sys.props("line.separator") + "=== Oracle ==="),
      cmd("getpublickey")
        .action((_, conf) => conf.copy(command = GetPublicKey))
        .text(s"Get oracle's public key"),
      cmd("getstakingaddress")
        .action((_, conf) => conf.copy(command = GetStakingAddress))
        .text(s"Get oracle's staking address"),
      cmd("listevents")
        .action((_, conf) => conf.copy(command = ListEvents))
        .text(s"Lists all event nonces"),
      cmd("createevent")
        .action((_, conf) =>
          conf.copy(command = CreateEvent("", Instant.MIN, Seq.empty)))
        .text("Registers an oracle event")
        .children(
          arg[String]("label")
            .text("Label for this event")
            .required()
            .action((label, conf) =>
              conf.copy(command = conf.command match {
                case createEvent: CreateEvent =>
                  createEvent.copy(label = label)
                case other => other
              })),
          arg[Instant]("maturationtime")
            .text("The earliest expected time an outcome will be signed, given in epoch second")
            .required()
            .action((time, conf) =>
              conf.copy(command = conf.command match {
                case createEvent: CreateEvent =>
                  createEvent.copy(maturationTime = time)
                case other => other
              })),
          arg[Seq[String]]("outcomes")
            .text("Possible outcomes for this event")
            .required()
            .action((outcomes, conf) =>
              conf.copy(command = conf.command match {
                case createEvent: CreateEvent =>
                  createEvent.copy(outcomes = outcomes)
                case other => other
              }))
        ),
      cmd("getevent")
        .action((_, conf) => conf.copy(command = GetEvent(null)))
        .text("Get an event's details")
        .children(
          arg[SchnorrNonce]("nonce")
            .text("Nonce associated with the event")
            .required()
            .action((nonce, conf) =>
              conf.copy(command = conf.command match {
                case getEvent: GetEvent =>
                  getEvent.copy(nonce = nonce)
                case other => other
              }))
        ),
      cmd("signevent")
        .action((_, conf) => conf.copy(command = SignEvent(null, "")))
        .text("Signs an event")
        .children(
          arg[SchnorrNonce]("nonce")
            .text("Nonce associated with the event to sign")
            .required()
            .action((nonce, conf) =>
              conf.copy(command = conf.command match {
                case signEvent: SignEvent =>
                  signEvent.copy(nonce = nonce)
                case other => other
              })),
          arg[String]("outcome")
            .text("Outcome to sign for this event")
            .required()
            .action((outcome, conf) =>
              conf.copy(command = conf.command match {
                case signEvent: SignEvent =>
                  signEvent.copy(outcome = outcome)
                case other => other
              }))
        ),
      cmd("getsignature")
        .action((_, conf) => conf.copy(command = GetSignature(null)))
        .text("Get the signature from a signed event")
        .children(
          arg[SchnorrNonce]("nonce")
            .text("Nonce associated with the signed event")
            .required()
            .action((nonce, conf) =>
              conf.copy(command = conf.command match {
                case getSignature: GetSignature =>
                  getSignature.copy(nonce = nonce)
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
      case GetDLCs => RequestParam("getdlcs")
      case GetDLC(paramHash) =>
        RequestParam("getdlc", Seq(up.writeJs(paramHash)))
      case CreateDLCOffer(oracleInfo,
                          contractInfo,
                          collateral,
                          feeRateOpt,
                          locktime,
                          refundLT) =>
        RequestParam(
          "createdlcoffer",
          Seq(
            up.writeJs(oracleInfo),
            up.writeJs(contractInfo),
            up.writeJs(collateral),
            up.writeJs(feeRateOpt),
            up.writeJs(locktime),
            up.writeJs(refundLT)
          )
        )
      case AcceptDLCOffer(offer) =>
        RequestParam("acceptdlcoffer", Seq(up.writeJs(offer)))
      case AcceptDLCOfferFromFile(path) =>
        RequestParam("acceptdlcofferfromfile", Seq(up.writeJs(path)))
      case SignDLC(accept) =>
        RequestParam("signdlc", Seq(up.writeJs(accept)))
      case SignDLCFromFile(path) =>
        RequestParam("signdlcfromfile", Seq(up.writeJs(path)))
      case AddDLCSigs(sigs) =>
        RequestParam("adddlcsigs", Seq(up.writeJs(sigs)))
      case AddDLCSigsFromFile(path) =>
        RequestParam("adddlcsigsfromfile", Seq(up.writeJs(path)))
      case ExecuteDLC(contractId, oracleSigs, noBroadcast) =>
        RequestParam("executedlc",
                     Seq(up.writeJs(contractId),
                         up.writeJs(oracleSigs),
                         up.writeJs(noBroadcast)))
      case GetDLCFundingTx(contractId) =>
        RequestParam("getdlcfundingtx", Seq(up.writeJs(contractId)))
      case BroadcastDLCFundingTx(contractId) =>
        RequestParam("broadcastdlcfundingtx", Seq(up.writeJs(contractId)))
      case ExecuteDLCRefund(contractId, noBroadcast) =>
        RequestParam("executedlcrefund",
                     Seq(up.writeJs(contractId), up.writeJs(noBroadcast)))
      // Wallet
      case GetBalance(isSats) =>
        RequestParam("getbalance", Seq(up.writeJs(isSats)))
      case GetConfirmedBalance(isSats) =>
        RequestParam("getconfirmedbalance", Seq(up.writeJs(isSats)))
      case GetUnconfirmedBalance(isSats) =>
        RequestParam("getunconfirmedbalance", Seq(up.writeJs(isSats)))
      case GetAddressInfo(address) =>
        RequestParam("getaddressinfo", Seq(up.writeJs(address)))
      case GetNewAddress(labelOpt) =>
        RequestParam("getnewaddress", Seq(up.writeJs(labelOpt)))
      case LockUnspent(unlock, outPoints) =>
        RequestParam("lockunspent",
                     Seq(up.writeJs(unlock), up.writeJs(outPoints)))
      case LabelAddress(address, label) =>
        RequestParam("labeladdress",
                     Seq(up.writeJs(address), up.writeJs(label)))
      case GetAddressTags(address) =>
        RequestParam("getaddresstags", Seq(up.writeJs(address)))
      case GetAddressLabels(address) =>
        RequestParam("getaddresslabels", Seq(up.writeJs(address)))
      case DropAddressLabels(address) =>
        RequestParam("dropaddresslabels", Seq(up.writeJs(address)))
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
        RequestParam("sendwithalgo",
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

      // Oracle
      case GetPublicKey =>
        RequestParam("getpublickey")
      case GetStakingAddress =>
        RequestParam("getstakingaddress")
      case ListEvents =>
        RequestParam("listevents")
      case GetEvent(nonce) =>
        RequestParam("getevent", Seq(up.writeJs(nonce)))
      case CreateEvent(label, time, outcomes) =>
        RequestParam(
          "createevent",
          Seq(up.writeJs(label), up.writeJs(time), up.writeJs(outcomes)))
      case SignEvent(nonce, outcome) =>
        RequestParam("signevent", Seq(up.writeJs(nonce), up.writeJs(outcome)))
      case GetSignature(nonce) =>
        RequestParam("getsignature", Seq(up.writeJs(nonce)))

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

      val jsObjT =
        Try(ujson.read(rawBody).obj)
          .transform[mutable.LinkedHashMap[String, ujson.Value]](
            Success(_),
            _ => error(s"Response was not a JSON object! Got: $rawBody"))

      /** Gets the given key from jsObj if it exists
        * and is not null
        */
      def getKey(key: String): Option[ujson.Value] = {
        jsObjT.toOption.flatMap(_.get(key).flatMap(result =>
          if (result.isNull) None else Some(result)))
      }

      /** Converts a `ujson.Value` to String, making an
        * effort to avoid preceding and trailing `"`s
        */
      def jsValueToString(value: ujson.Value) =
        value match {
          case Str(string)             => string
          case Num(num) if num.isWhole => num.toLong.toString
          case Num(num)                => num.toString
          case rest: ujson.Value       => rest.render(2)
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

  trait Broadcastable {
    def noBroadcast: Boolean
  }

  // DLC
  case class CreateDLCOffer(
      oracleInfo: OracleInfo,
      contractInfo: ContractInfoTLV,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLT: UInt32)
      extends CliCommand

  sealed trait AcceptDLCCliCommand extends CliCommand

  case class AcceptDLCOffer(offer: LnMessage[DLCOfferTLV])
      extends AcceptDLCCliCommand

  case class AcceptDLCOfferFromFile(path: Path) extends AcceptDLCCliCommand

  sealed trait SignDLCCliCommand extends CliCommand

  case class SignDLC(accept: LnMessage[DLCAcceptTLV]) extends SignDLCCliCommand

  case class SignDLCFromFile(path: Path) extends SignDLCCliCommand

  sealed trait AddDLCSigsCliCommand extends CliCommand

  case class AddDLCSigs(sigs: LnMessage[DLCSignTLV])
      extends AddDLCSigsCliCommand

  case class AddDLCSigsFromFile(path: Path) extends AddDLCSigsCliCommand

  case class GetDLCFundingTx(contractId: ByteVector) extends CliCommand

  case class BroadcastDLCFundingTx(contractId: ByteVector) extends CliCommand

  case class ExecuteDLC(
      contractId: ByteVector,
      oracleSigs: Vector[SchnorrDigitalSignature],
      noBroadcast: Boolean)
      extends CliCommand
      with Broadcastable

  case class ExecuteDLCRefund(contractId: ByteVector, noBroadcast: Boolean)
      extends CliCommand
      with Broadcastable

  case object GetDLCs extends CliCommand
  case class GetDLC(paramHash: Sha256DigestBE) extends CliCommand

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

  case class LockUnspent(
      unlock: Boolean,
      outPoints: Vector[LockUnspentOutputParameter])
      extends CliCommand

  case class LabelAddress(address: BitcoinAddress, label: AddressLabelTag)
      extends CliCommand

  case class GetAddressTags(address: BitcoinAddress) extends CliCommand

  case class GetAddressLabels(address: BitcoinAddress) extends CliCommand

  case class DropAddressLabels(address: BitcoinAddress) extends CliCommand

  case class GetNewAddress(labelOpt: Option[AddressLabelTag]) extends CliCommand
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

  // Oracle
  case object GetPublicKey extends CliCommand
  case object GetStakingAddress extends CliCommand
  case object ListEvents extends CliCommand

  case class GetEvent(nonce: SchnorrNonce) extends CliCommand

  case class CreateEvent(
      label: String,
      maturationTime: Instant,
      outcomes: Seq[String])
      extends CliCommand
  case class SignEvent(nonce: SchnorrNonce, outcome: String) extends CliCommand
  case class GetSignature(nonce: SchnorrNonce) extends CliCommand
}
