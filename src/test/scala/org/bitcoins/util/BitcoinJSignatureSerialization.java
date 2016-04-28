package org.bitcoins.util;


import org.bitcoinj.core.*;
import org.bitcoinj.params.TestNet3Params;
import org.bitcoinj.script.Script;
import org.bitcoinj.script.ScriptOpCodes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.List;

import static org.bitcoinj.core.Utils.uint32ToByteStreamLE;

/**
 * Created by chris on 2/24/16.
 */
public class BitcoinJSignatureSerialization {
    public static final Logger logger = LoggerFactory.getLogger("BitcoinJSignatureSerialization");

    /**
     * This is required for signatures which use a sigHashType which cannot be represented using SigHash and anyoneCanPay
     * See transaction c99c49da4c38af669dea436d3e73780dfdb6c1ecf9958baa52960e8baee30e73, which has sigHashType 0
     */
    public static synchronized byte[] serializeForSignature(Transaction spendingTx, int inputIndex, byte[] connectedScript,
                                                            byte sigHashType) {
        NetworkParameters params = TestNet3Params.get();
        // The SIGHASH flags are used in the design of contracts, please see this page for a further understanding of
        // the purposes of the code in this method:
        //
        //   https://en.bitcoin.it/wiki/Contracts

        try {

            Transaction tx = new Transaction(params, spendingTx.bitcoinSerialize());
            // Store all the input scripts and clear them in preparation for signing. If we're signing a fresh
            // transaction that step isn't very helpful, but it doesn't add much cost relative to the actual
            // EC math so we'll do it anyway.
            //
            // Also store the input sequence numbers in case we are clearing them with SigHash.NONE/SINGLE

            byte[][] inputScripts = new byte[tx.getInputs().size()][];
            long[] inputSequenceNumbers = new long[tx.getInputs().size()];
            for (int i = 0; i < tx.getInputs().size(); i++) {
                inputScripts[i] = tx.getInputs().get(i).getScriptBytes();
                inputSequenceNumbers[i] = tx.getInputs().get(i).getSequenceNumber();
                tx.getInput(i).setScriptSig(new Script(new byte[0]));
            }

            // This step has no purpose beyond being synchronized with the reference clients bugs. OP_CODESEPARATOR
            // is a legacy holdover from a previous, broken design of executing scripts that shipped in Bitcoin 0.1.
            // It was seriously flawed and would have let anyone take anyone elses money. Later versions switched to
            // the design we use today where scripts are executed independently but share a stack. This left the
            // OP_CODESEPARATOR instruction having no purpose as it was only meant to be used internally, not actually
            // ever put into scripts. Deleting OP_CODESEPARATOR is a step that should never be required but if we don't
            // do it, we could split off the main chain.

            connectedScript = Script.removeAllInstancesOfOp(connectedScript, ScriptOpCodes.OP_CODESEPARATOR);
            // Set the input to the script of its output. Satoshi does this but the step has no obvious purpose as
            // the signature covers the hash of the prevout transaction which obviously includes the output script
            // already. Perhaps it felt safer to him in some way, or is another leftover from how the code was written.
            TransactionInput input = tx.getInputs().get(inputIndex);
            input.setScriptSig(new Script(connectedScript));
            List<TransactionOutput> outputs = tx.getOutputs();



            if ((sigHashType & 0x1f) == (Transaction.SigHash.NONE.ordinal() + 1)) {
                // SIGHASH_NONE means no outputs are signed at all - the signature is effectively for a "blank cheque".
                //this.outputs = new ArrayList<TransactionOutput>(0);
                tx.clearOutputs();
                // The signature isn't broken by new versions of the transaction issued by other parties.
                for (int i = 0; i < tx.getInputs().size(); i++)
                    if (i != inputIndex)
                        tx.getInputs().get(i).setSequenceNumber(0);
            } else if ((sigHashType & 0x1f) == (Transaction.SigHash.SINGLE.ordinal() + 1)) {
                // SIGHASH_SINGLE means only sign the output at the same index as the input (ie, my output).
                if (inputIndex >= tx.getOutputs().size()) {
                    // The input index is beyond the number of outputs, it's a buggy signature made by a broken
                    // Bitcoin implementation. The reference client also contains a bug in handling this case:
                    // any transaction output that is signed in this case will result in both the signed output
                    // and any future outputs to this public key being steal-able by anyone who has
                    // the resulting signature and the public key (both of which are part of the signed tx input).
                    // Put the transaction back to how we found it.
                    //
                    // TODO: Only allow this to happen if we are checking a signature, not signing a transactions
                    for (int i = 0; i < tx.getInputs().size(); i++) {
                        //tx.getInputs().get(i).setScriptSig(inputScripts[i]);
/*                        tx.getInputs().get(i).setScriptSig(ScriptBuilder.createMultiSigInputScriptBytes(
                                Arrays.asList(inputScripts[i])));*/
                        tx.getInput(i).setScriptSig(new Script(inputScripts[i]));
                        tx.getInputs().get(i).setSequenceNumber(inputSequenceNumbers[i]);
                    }
                    //this.outputs = outputs;

                    // Satoshis bug is that SignatureHash was supposed to return a hash and on this codepath it
                    // actually returns the constant "1" to indicate an error, which is never checked for. Oops.
                    return Utils.HEX.decode("0100000000000000000000000000000000000000000000000000000000000000");
                }

                // In SIGHASH_SINGLE the outputs after the matching input index are deleted, and the outputs before
                // that position are "nulled out". Unintuitively, the value in a "null" transaction is set to -1.
/*                this.outputs = new ArrayList<TransactionOutput>(this.outputs.subList(0, inputIndex + 1));
                for (int i = 0; i < inputIndex; i++)
                    this.outputs.set(i, new TransactionOutput(params, this, Coin.NEGATIVE_SATOSHI, new byte[] {}));
                // The signature isn't broken by new versions of the transaction issued by other parties.
                for (int i = 0; i < inputs.size(); i++)
                    if (i != inputIndex)
                        inputs.get(i).setSequenceNumber(0);*/
                // In SIGHASH_SINGLE the outputs after the matching input index are deleted, and the outputs before
                // that position are "nulled out". Unintuitively, the value in a "null" transaction is set to -1.
                //tx.outputs = new ArrayList<TransactionOutput>(tx.getOutputs().subList(0, inputIndex + 1));
                tx.clearOutputs();
                for (int i = 0; i <= inputIndex; i++)
                    if (i == inputIndex) {
                        //need to make sure the output at inputIndex stays the same
                        tx.addOutput(spendingTx.getOutput(inputIndex));
                    } else {
                        //this.outputs.set(i, new TransactionOutput(params, this, Coin.NEGATIVE_SATOSHI, new byte[] {}));
                        tx.addOutput(new TransactionOutput(params, tx, Coin.NEGATIVE_SATOSHI, new byte[] {}));
                    }

                // The signature isn't broken by new versions of the transaction issued by other parties.
                for (int i = 0; i < tx.getInputs().size(); i++)
                    if (i != inputIndex)
                        tx.getInputs().get(i).setSequenceNumber(0);

            }

            List<TransactionInput> inputs = tx.getInputs();
            if ((sigHashType & (byte)0x80) == 0x80) {
                // SIGHASH_ANYONECANPAY means the signature in the input is not broken by changes/additions/removals
                // of other inputs. For example, this is useful for building assurance contracts.
                tx.clearInputs();
                tx.getInputs().add(input);
            }

            ByteArrayOutputStream bos = new UnsafeByteArrayOutputStream(256);
            tx.bitcoinSerialize(bos);
            // We also have to write a hash type (sigHashType is actually an unsigned char)
            uint32ToByteStreamLE(0x000000ff & sigHashType, bos);
            // Note that this is NOT reversed to ensure it will be signed correctly. If it were to be printed out
            // however then we would expect that it is IS reversed.
            byte[] txSignatureBytes = bos.toByteArray();
            bos.close();

            // Put the transaction back to how we found it.
            //tx.inputs = inputs;
            tx.clearInputs();
            for (int i = 0; i < inputs.size(); i ++ ) {
                tx.addInput(inputs.get(i));
            }
            for (int i = 0; i < inputs.size(); i++) {
                inputs.get(i).setScriptSig(new Script(inputScripts[i]));
                inputs.get(i).setSequenceNumber(inputSequenceNumbers[i]);
            }
            //this.outputs = outputs;
            tx.clearOutputs();
            for (int i = 0; i < outputs.size(); i++) {
                tx.addOutput(outputs.get(i));
            }
            return txSignatureBytes;
        } catch (IOException e) {
            throw new RuntimeException(e);  // Cannot happen.
        }
    }


    public static byte[] hashForSignature(Transaction tx, int inputIndex, byte[] connectedScript, byte sigHashType) {
        byte[] serializedSig = serializeForSignature(tx,inputIndex,connectedScript, sigHashType);

        Sha256Hash hash = Sha256Hash.twiceOf(serializedSig);
        String hashHex = Utils.HEX.encode(hash.getBytes());
        //check hash against the reference implementation inside of bitcoinj
        byte[] referenceImplementation = tx.hashForSignature(inputIndex,connectedScript,sigHashType).getBytes();
        String referenceImplementationHex = Utils.HEX.encode(referenceImplementation);
        if (!hashHex.equals(referenceImplementationHex))  {
            logger.error("bitcoins: " + hashHex);
            logger.error("bitcoinj: " + referenceImplementationHex );
            throw new RuntimeException("Difference between BitcoinJSignatureSerialization & Actual Bitcoinj\n" +
                "bitcoin-s: " + hashHex + "\n" +
                "bitcoin-j: " + referenceImplementationHex);
        }
        return hash.getBytes();
    }

}
