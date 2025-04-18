/*
 * Copyright 2013 Google Inc.
 * Copyright 2014-2016 the libsecp256k1 contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.bitcoin;

import java.math.BigInteger;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import static org.bitcoin.NativeSecp256k1Util.*;

/**
 * <p>This class holds native methods to handle ECDSA verification.</p>
 *
 * <p>You can find an example library that can be used for this at https://github.com/bitcoin-core/secp256k1</p>
 *
 * <p>To build secp256k1 for use with bitcoinj, run
 * `./configure --enable-jni --enable-experimental --enable-module-ecdh`
 * and `make` then copy `.libs/libsecp256k1.so` to your system library path
 * or point the JVM to the folder containing it with -Djava.library.path
 * </p>
 */
public class NativeSecp256k1 {

    private static final ReentrantReadWriteLock rwl = new ReentrantReadWriteLock();
    private static final Lock r = rwl.readLock();
    private static final Lock w = rwl.writeLock();
    private static ThreadLocal<ByteBuffer> nativeECDSABuffer = new ThreadLocal<ByteBuffer>();
    /**
     * Verifies the given secp256k1 signature in native code.
     * Calling when enabled == false is undefined (probably library not loaded)
     *
     * @param data The data which was signed, must be exactly 32 bytes
     * @param signature The signature
     * @param pub The public key which did the signing
     */
    public static boolean verify(byte[] data, byte[] signature, byte[] pub) {
        checkArgument(data.length == 32 && signature.length <= 520 && pub.length <= 520);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < 520) {
            byteBuff = ByteBuffer.allocateDirect(520);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }

        safeRewind(byteBuff);
        byteBuff.put(data);
        byteBuff.put(signature);
        byteBuff.put(pub);

        r.lock();
        try {
          return secp256k1_ecdsa_verify(byteBuff, Secp256k1Context.getContext(), signature.length, pub.length) == 1;
        } finally {
          r.unlock();
        }
    }

    /**
     * libsecp256k1 Create an ECDSA signature.
     *
     * @param data Message hash, 32 bytes
     * @param seckey ECDSA Secret key, 32 bytes
     * @return sig byte array of signature
     */
    public static byte[] sign(byte[] data, byte[] seckey) throws AssertFailException{
        checkArgument(data.length == 32 && seckey.length <= 32);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < 32 + 32) {
            byteBuff = ByteBuffer.allocateDirect(32 + 32);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }

        safeRewind(byteBuff);
        byteBuff.put(data);
        byteBuff.put(seckey);

        byte[][] retByteArray;

        r.lock();
        try {
          retByteArray = secp256k1_ecdsa_sign(byteBuff, Secp256k1Context.getContext());
        } finally {
          r.unlock();
        }

        byte[] sigArr = retByteArray[0];
        int sigLen = new BigInteger(new byte[] { retByteArray[1][0] }).intValue();
        int retVal = new BigInteger(new byte[] { retByteArray[1][1] }).intValue();

        assertEquals(sigArr.length, sigLen, "Got bad signature length.");

        return retVal == 0 ? new byte[0] : sigArr;
    }

    /**
     * libsecp256k1 Create an ECDSA signature adding specified entropy.
     *
     * This can be used to include your own entropy to nonce generation
     * in addition to the message and private key, while still doing so deterministically.
     *
     * In particular, this is used when generating low R signatures.
     * See https://github.com/bitcoin/bitcoin/pull/13666/
     *
     * @param data Message hash, 32 bytes
     * @param seckey ECDSA Secret key, 32 bytes
     * @param entropy 32 bytes of entropy
     * @return sig byte array of signature
     */
    public static byte[] signWithEntropy(byte[] data, byte[] seckey, byte[] entropy) throws AssertFailException{
        checkArgument(data.length == 32 && seckey.length == 32 && entropy.length == 32);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < 32 + 32 + 32) {
            byteBuff = ByteBuffer.allocateDirect(32 + 32 + 32);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }
        safeRewind(byteBuff);
        byteBuff.put(data);
        byteBuff.put(seckey);
        byteBuff.put(entropy);

        byte[][] retByteArray;

        r.lock();
        try {
            retByteArray = secp256k1_ecdsa_sign_with_entropy(byteBuff, Secp256k1Context.getContext());
        } finally {
            r.unlock();
        }

        byte[] sigArr = retByteArray[0];
        int sigLen = new BigInteger(new byte[] { retByteArray[1][0] }).intValue();
        int retVal = new BigInteger(new byte[] { retByteArray[1][1] }).intValue();

        assertEquals(sigArr.length, sigLen, "Got bad signature length.");

        return retVal == 0 ? new byte[0] : sigArr;
    }

    /**
     * libsecp256k1 Seckey Verify - Verifies an ECDSA secret key
     *
     * @param seckey ECDSA Secret key, 32 bytes
     * @return true if valid, false if invalid
     */
    public static boolean secKeyVerify(byte[] seckey) {
        checkArgument(seckey.length == 32);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < seckey.length) {
            byteBuff = ByteBuffer.allocateDirect(seckey.length);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }

        safeRewind(byteBuff);
        byteBuff.put(seckey);

        r.lock();
        try {
          return secp256k1_ec_seckey_verify(byteBuff,Secp256k1Context.getContext()) == 1;
        } finally {
          r.unlock();
        }
    }


    /**
     * libsecp256k1 Compute Pubkey - computes public key from secret key
     *
     * @param seckey ECDSA Secret key, 32 bytes
     * @param compressed Should the generated public key be compressed
     * @return pubkey ECDSA Public key, 33 or 65 bytes
     */
    public static byte[] computePubkey(byte[] seckey, boolean compressed) throws AssertFailException{
        checkArgument(seckey.length == 32);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < seckey.length) {
            byteBuff = ByteBuffer.allocateDirect(seckey.length);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }

        safeRewind(byteBuff);
        byteBuff.put(seckey);

        byte[][] retByteArray;

        r.lock();
        try {
          retByteArray = secp256k1_ec_pubkey_create(byteBuff, Secp256k1Context.getContext(), compressed);
        } finally {
          r.unlock();
        }

        byte[] pubArr = retByteArray[0];
        int pubLen = new BigInteger(new byte[] { retByteArray[1][0] }).intValue();
        int retVal = new BigInteger(new byte[] { retByteArray[1][1] }).intValue();

        assertEquals(pubArr.length, pubLen, "Got bad pubkey length.");

        return retVal == 0 ? new byte[0]: pubArr;
    }

    /**
     * libsecp256k1 Cleanup - This destroys the secp256k1 context object
     * This should be called at the end of the program for proper cleanup of the context.
     */
    public static synchronized void cleanup() {
        w.lock();
        try {
          secp256k1_destroy_context(Secp256k1Context.getContext());
        } finally {
          w.unlock();
        }
    }

    public static long cloneContext() {
       r.lock();
       try {
        return secp256k1_ctx_clone(Secp256k1Context.getContext());
       } finally { r.unlock(); }
    }

    /**
     * libsecp256k1 PrivKey Tweak-Mul - Tweak seckey by multiplying to it
     *
     * @param seckey ECDSA Secret key, 32 bytes
     * @param tweak some bytes to tweak with
     */
    public static byte[] privKeyTweakMul(byte[] seckey, byte[] tweak) throws AssertFailException{
        checkArgument(seckey.length == 32);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < seckey.length + tweak.length) {
            byteBuff = ByteBuffer.allocateDirect(seckey.length + tweak.length);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }

        safeRewind(byteBuff);
        byteBuff.put(seckey);
        byteBuff.put(tweak);

        byte[][] retByteArray;
        r.lock();
        try {
          retByteArray = secp256k1_privkey_tweak_mul(byteBuff,Secp256k1Context.getContext());
        } finally {
          r.unlock();
        }

        byte[] privArr = retByteArray[0];

        int privLen = (byte) new BigInteger(new byte[] { retByteArray[1][0] }).intValue() & 0xFF;
        int retVal = new BigInteger(new byte[] { retByteArray[1][1] }).intValue();

        assertEquals(privArr.length, privLen, "Got bad pubkey length.");

        assertEquals(retVal, 1, "Failed return value check.");

        return privArr;
    }

    /**
     * libsecp256k1 PrivKey Tweak-Add - Tweak seckey by adding to it
     *
     * @param seckey ECDSA Secret key, 32 bytes
     * @param tweak some bytes to tweak with
     */
    public static byte[] privKeyTweakAdd(byte[] seckey, byte[] tweak) throws AssertFailException{
        checkArgument(seckey.length == 32);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < seckey.length + tweak.length) {
            byteBuff = ByteBuffer.allocateDirect(seckey.length + tweak.length);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }

        safeRewind(byteBuff);
        byteBuff.put(seckey);
        byteBuff.put(tweak);

        byte[][] retByteArray;
        r.lock();
        try {
          retByteArray = secp256k1_privkey_tweak_add(byteBuff,Secp256k1Context.getContext());
        } finally {
          r.unlock();
        }

        byte[] privArr = retByteArray[0];

        int privLen = (byte) new BigInteger(new byte[] { retByteArray[1][0] }).intValue() & 0xFF;
        int retVal = new BigInteger(new byte[] { retByteArray[1][1] }).intValue();

        assertEquals(privArr.length, privLen, "Got bad pubkey length.");

        assertEquals(retVal, 1, "Failed return value check.");

        return privArr;
    }

    /**
     * libsecp256k1 PubKey Tweak-Add - Tweak pubkey by adding to it
     *
     * @param pubkey ECDSA Public key, 33 or 65 bytes
     * @param tweak some bytes to tweak with
     * @param compressed should the output public key be compressed
     */
    public static byte[] pubKeyTweakAdd(byte[] pubkey, byte[] tweak, boolean compressed) throws AssertFailException{
        checkArgument(pubkey.length == 33 || pubkey.length == 65);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < pubkey.length + tweak.length) {
            byteBuff = ByteBuffer.allocateDirect(pubkey.length + tweak.length);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }

        safeRewind(byteBuff);
        byteBuff.put(pubkey);
        byteBuff.put(tweak);

        byte[][] retByteArray;
        r.lock();
        try {
          retByteArray = secp256k1_pubkey_tweak_add(byteBuff, Secp256k1Context.getContext(), pubkey.length, compressed);
        } finally {
          r.unlock();
        }

        byte[] pubArr = retByteArray[0];

        int pubLen = (byte) new BigInteger(new byte[] { retByteArray[1][0] }).intValue() & 0xFF;
        int retVal = new BigInteger(new byte[] { retByteArray[1][1] }).intValue();

        assertEquals(pubArr.length, pubLen, "Got bad pubkey length.");

        assertEquals(retVal, 1, "Failed return value check.");

        return pubArr;
    }

    /**
     * libsecp256k1 PubKey Tweak-Mul - Tweak pubkey by multiplying to it
     *
     * @param pubkey ECDSA Public key, 33 or 65 bytes
     * @param tweak some bytes to tweak with
     * @param compressed should the output public key be compressed
     */
    public static byte[] pubKeyTweakMul(byte[] pubkey, byte[] tweak, boolean compressed) throws AssertFailException{
        checkArgument(pubkey.length == 33 || pubkey.length == 65);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < pubkey.length + tweak.length) {
            byteBuff = ByteBuffer.allocateDirect(pubkey.length + tweak.length);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }

        safeRewind(byteBuff);
        byteBuff.put(pubkey);
        byteBuff.put(tweak);

        byte[][] retByteArray;
        r.lock();
        try {
          retByteArray = secp256k1_pubkey_tweak_mul(byteBuff,Secp256k1Context.getContext(), pubkey.length, compressed);
        } finally {
          r.unlock();
        }

        byte[] pubArr = retByteArray[0];

        int pubLen = (byte) new BigInteger(new byte[] { retByteArray[1][0] }).intValue() & 0xFF;
        int retVal = new BigInteger(new byte[] { retByteArray[1][1] }).intValue();

        assertEquals(pubArr.length, pubLen, "Got bad pubkey length.");

        assertEquals(retVal, 1, "Failed return value check.");

        return pubArr;
    }

    /**
     * libsecp256k1 PubKey Combine - Add pubkeys together
     *
     * @param pubkeys array of ECDSA Public key, 33 or 65 bytes each
     * @param compressed should the output public key be compressed
     */
    public static byte[] pubKeyCombine(byte[][] pubkeys, boolean compressed) throws AssertFailException{
        int numKeys = pubkeys.length;
        checkArgument(numKeys > 0);

        int pubkeyLength = pubkeys[0].length;
        checkArgument(pubkeyLength == 33 || pubkeyLength == 65);
        
        for (byte[] pubkey : pubkeys) {
            checkArgument(pubkey.length == pubkeyLength);
        }
        
        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < numKeys * pubkeyLength) {
            byteBuff = ByteBuffer.allocateDirect(numKeys * pubkeyLength);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }

        safeRewind(byteBuff);
        for (byte[] pubkey : pubkeys) {
            byteBuff.put(pubkey);
        }

        byte[][] retByteArray;
        r.lock();
        try {
          retByteArray = secp256k1_ec_pubkey_combine(byteBuff,Secp256k1Context.getContext(), pubkeyLength, numKeys, compressed);
        } finally {
          r.unlock();
        }

        byte[] pubArr = retByteArray[0];
        
        int pubLen = (byte) new BigInteger(new byte[] { retByteArray[1][0] }).intValue() & 0xFF;
        int retVal = new BigInteger(new byte[] { retByteArray[1][1] }).intValue();
        
        assertEquals(pubArr.length, pubLen, "Got bad pubkey length.");
        
        assertEquals(retVal, 1, "Failed return value check.");
        
        return pubArr;
    }

    /**
     * libsecp256k1 Decompress - Parse and decompress a variable-length pub key
     *
     * @param pubkey ECDSA Public key, 33 or 65 bytes
     */
    public static byte[] decompress(byte[] pubkey) throws AssertFailException{
        checkArgument(pubkey.length == 33 || pubkey.length == 65);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < pubkey.length) {
            byteBuff = ByteBuffer.allocateDirect(pubkey.length);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }

        safeRewind(byteBuff);
        byteBuff.put(pubkey);

        byte[][] retByteArray;
        r.lock();
        try {
            retByteArray = secp256k1_ec_pubkey_decompress(byteBuff, Secp256k1Context.getContext(), pubkey.length);
        } finally {
            r.unlock();
        }

        byte[] pubArr = retByteArray[0];

        int pubLen = (byte) new BigInteger(new byte[] { retByteArray[1][0] }).intValue() & 0xFF;
        int retVal = new BigInteger(new byte[] { retByteArray[1][1] }).intValue();

        assertEquals(pubArr.length, pubLen, "Got bad pubkey length.");

        assertEquals(retVal, 1, "Failed return value check.");

        return pubArr;
    }

    /**
     * libsecp256k1 IsValidPubKey - Checks if a pubkey is valid
     *
     * @param pubkey ECDSA Public key, 33 or 65 bytes
     */
    public static boolean isValidPubKey(byte[] pubkey) {
        if (!(pubkey.length == 33 || pubkey.length == 65)) {
            return false;
        }

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < pubkey.length) {
            byteBuff = ByteBuffer.allocateDirect(pubkey.length);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }

        safeRewind(byteBuff);
        byteBuff.put(pubkey);

        byte[][] retByteArray;
        r.lock();
        try {
            retByteArray = secp256k1_ec_pubkey_decompress(byteBuff, Secp256k1Context.getContext(), pubkey.length);
        } finally {
            r.unlock();
        }

        int retVal = new BigInteger(new byte[] { retByteArray[1][1] }).intValue();

        return retVal == 1;
    }

    /**
     * libsecp256k1 create ECDH secret - constant time ECDH calculation
     *
     * @param seckey byte array of secret key used in exponentiaion
     * @param pubkey byte array of public key used in exponentiaion
     */
    public static byte[] createECDHSecret(byte[] seckey, byte[] pubkey) throws AssertFailException{
        checkArgument(seckey.length <= 32 && pubkey.length <= 65);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < 32 + pubkey.length) {
            byteBuff = ByteBuffer.allocateDirect(32 + pubkey.length);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }

        safeRewind(byteBuff);
        byteBuff.put(seckey);
        byteBuff.put(pubkey);

        byte[][] retByteArray;
        r.lock();
        try {
          retByteArray = secp256k1_ecdh(byteBuff, Secp256k1Context.getContext(), pubkey.length);
        } finally {
          r.unlock();
        }

        byte[] resArr = retByteArray[0];
        int retVal = new BigInteger(new byte[] { retByteArray[1][0] }).intValue();

        assertEquals(resArr.length, 32, "Got bad result length.");
        assertEquals(retVal, 1, "Failed return value check.");

        return resArr;
    }

    /**
     * libsecp256k1 schnorr sign - generates a BIP 340 Schnorr signature
     *
     * @param data message to sign
     * @param secKey key to sign with
     */
    public static byte[] schnorrSign(byte[] data, byte[] secKey, byte[] auxRand) throws AssertFailException {
        checkArgument(secKey.length == 32 && auxRand.length == 32);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < 32 + 32 + 32) {
            byteBuff = ByteBuffer.allocateDirect(32 + 32 + 32);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }
        byteBuff.rewind();
        byteBuff.put(data);
        byteBuff.put(secKey);
        byteBuff.put(auxRand);

        byte[][] retByteArray;
        r.lock();
        try {
            retByteArray = secp256k1_schnorrsig_sign(byteBuff, Secp256k1Context.getContext());
        } finally {
            r.unlock();
        }

        byte[] sigArray = retByteArray[0];
        int retVal = new BigInteger(new byte[]{retByteArray[1][0]}).intValue();

        assertEquals(retVal, 1, "Failed return value check.");

        return sigArray;
    }

    /**
     * libsecp256k1 schnorr sign - generates a BIP 340 Schnorr signature
     *
     * @param data message to sign
     * @param secKey key to sign with
     * @param nonce the nonce (k value) used in signing
     */
    public static byte[] schnorrSignWithNonce(byte[] data, byte[] secKey, byte[] nonce) throws AssertFailException {
        checkArgument(secKey.length == 32 && nonce.length == 32);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < 32 + 32 + 32) {
            byteBuff = ByteBuffer.allocateDirect(32 + 32 + 32);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }
        byteBuff.rewind();
        byteBuff.put(data);
        byteBuff.put(secKey);
        byteBuff.put(nonce);

        byte[][] retByteArray;
        r.lock();
        try {
            retByteArray = secp256k1_schnorrsig_sign_with_nonce(byteBuff, Secp256k1Context.getContext());
        } finally {
            r.unlock();
        }

        byte[] sigArray = retByteArray[0];
        int retVal = new BigInteger(new byte[]{retByteArray[1][0]}).intValue();

        assertEquals(retVal, 1, "Failed return value check.");

        return sigArray;
    }

    /*
    public static byte[] schnorrComputeSigPoint(byte[] data, byte[] nonce, byte[] pubkey, boolean compressed) throws AssertFailException {
        checkArgument(data.length == 32 && nonce.length == 32 && pubkey.length == 32);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < 32 + 32 + 32) {
            byteBuff = ByteBuffer.allocateDirect(32 + 32 + 32);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }
        byteBuff.rewind();
        byteBuff.put(data);
        byteBuff.put(nonce);
        byteBuff.put(pubkey);

        byte[][] retByteArray;
        r.lock();
        try {
            retByteArray = secp256k1_schnorrsig_compute_sigpoint(byteBuff, Secp256k1Context.getContext(), compressed);
        } finally {
            r.unlock();
        }

        byte[] pointArray = retByteArray[0];
        int outputLen = new BigInteger(new byte[] { retByteArray[1][0] }).intValue() & 0xFF;
        int retVal = new BigInteger(new byte[] { retByteArray[1][1] }).intValue();

        assertEquals(pointArray.length, outputLen, "Got bad point length.");
        assertEquals(retVal, 1, "Failed return value check.");

        return pointArray;
    }
     */

    /**
     * libsecp256k1 schnorr verify - verifies BIP 340 Schnorr signatures
     *
     * @param sig signature to verify
     * @param data message the signature has signed
     * @param pubx the key that did the signing
     */
    public static boolean schnorrVerify(byte[] sig, byte[] data, byte[] pubx) throws AssertFailException {
        checkArgument(sig.length == 64 && pubx.length == 32);

        ByteBuffer byteBuffer = nativeECDSABuffer.get();
        if (byteBuffer == null || byteBuffer.capacity() < 64 + 32 + 32) {
            byteBuffer = ByteBuffer.allocateDirect(64 + 32 + 32);
            byteBuffer.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuffer);
        }
        byteBuffer.rewind();
        byteBuffer.put(sig);
        byteBuffer.put(data);
        byteBuffer.put(pubx);

        r.lock();
        try {
            return secp256k1_schnorrsig_verify(byteBuffer, Secp256k1Context.getContext()) == 1;
        } finally {
            r.unlock();
        }
    }

    public static byte[] adaptorSign(byte[] seckey, byte[] adaptorPoint, byte[] data, byte[] auxRand) throws AssertFailException{
        checkArgument(seckey.length == 32 &&
                data.length == 32 &&
                (adaptorPoint.length == 33 || adaptorPoint.length == 65) &&
                auxRand.length == 32);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < 32 + 32 + adaptorPoint.length + 32) {
            byteBuff = ByteBuffer.allocateDirect(32 + 32 + adaptorPoint.length + 32);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }
        byteBuff.rewind();
        byteBuff.put(seckey);
        byteBuff.put(adaptorPoint);
        byteBuff.put(data);
        byteBuff.put(auxRand);

        byte[][] retByteArray;

        r.lock();
        try {
            retByteArray = secp256k1_ecdsa_adaptor_sign(byteBuff, Secp256k1Context.getContext(), adaptorPoint.length);
        } finally {
            r.unlock();
        }

        byte[] sigArr = retByteArray[0];
        int retVal = new BigInteger(new byte[] { retByteArray[1][0] }).intValue();

        if (retVal == 0) {
            return new byte[]{};
        } else {
            return sigArr;
        }
    }

    public static boolean adaptorVerify(byte[] adaptorSig, byte[] pubKey, byte[] data, byte[] adaptorPoint) throws AssertFailException{
        checkArgument(data.length == 32 &&
                adaptorSig.length == 162 &&
                (pubKey.length == 33 || pubKey.length == 65) &&
                adaptorPoint.length == pubKey.length);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        int buffLen = 32 + 162 + pubKey.length + adaptorPoint.length;
        if (byteBuff == null || byteBuff.capacity() < buffLen) {
            byteBuff = ByteBuffer.allocateDirect(buffLen);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }
        byteBuff.rewind();
        byteBuff.put(adaptorSig);
        byteBuff.put(pubKey);
        byteBuff.put(data);
        byteBuff.put(adaptorPoint);

        r.lock();
        try {
            return secp256k1_ecdsa_adaptor_sig_verify(byteBuff, Secp256k1Context.getContext(), pubKey.length) == 1;
        } finally {
            r.unlock();
        }
    }

    public static byte[] adaptorAdapt(byte[] adaptorSec, byte[] adaptorSig) throws AssertFailException{
        checkArgument(adaptorSec.length == 32 && adaptorSig.length == 162);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < 32 + 162) {
            byteBuff = ByteBuffer.allocateDirect(32 + 162);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }
        byteBuff.rewind();
        byteBuff.put(adaptorSec);
        byteBuff.put(adaptorSig);

        byte[][] retByteArray;

        r.lock();
        try {
            retByteArray = secp256k1_ecdsa_adaptor_adapt(byteBuff, Secp256k1Context.getContext());
        } finally {
            r.unlock();
        }

        byte[] sigArr = retByteArray[0];
        int sigLen = new BigInteger(new byte[] { retByteArray[1][0] }).intValue();
        int retVal = new BigInteger(new byte[] { retByteArray[1][1] }).intValue();

        assertEquals(sigArr.length, sigLen, "Got bad signature length.");

        return retVal == 0 ? new byte[0] : sigArr;
    }

    public static byte[] adaptorExtractSecret(byte[] sig, byte[] adaptorSig, byte[] adaptor) throws AssertFailException{
        checkArgument(sig.length <= 520 && (adaptor.length == 33 || adaptor.length == 65) && adaptorSig.length == 162);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < sig.length + adaptor.length + 162) {
            byteBuff = ByteBuffer.allocateDirect(sig.length + adaptor.length + 162);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }
        byteBuff.rewind();
        byteBuff.put(sig);
        byteBuff.put(adaptorSig);
        byteBuff.put(adaptor);

        byte[][] retByteArray;

        r.lock();
        try {
            retByteArray = secp256k1_ecdsa_adaptor_extract_secret(byteBuff, Secp256k1Context.getContext(), sig.length, adaptor.length);
        } finally {
            r.unlock();
        }

        byte[] sigArr = retByteArray[0];
        int retVal = new BigInteger(new byte[] { retByteArray[1][0] }).intValue();

        return retVal == 0 ? new byte[0] : sigArr;
    }

    /**
     * libsecp256k1 randomize - updates the context randomization
     *
     * @param seed 32-byte random seed
     */
    public static synchronized boolean randomize(byte[] seed) throws AssertFailException{
        checkArgument(seed.length == 32);

        ByteBuffer byteBuff = nativeECDSABuffer.get();
        if (byteBuff == null || byteBuff.capacity() < seed.length) {
            byteBuff = ByteBuffer.allocateDirect(seed.length);
            byteBuff.order(ByteOrder.nativeOrder());
            nativeECDSABuffer.set(byteBuff);
        }
        safeRewind(byteBuff);
        byteBuff.put(seed);

        w.lock();
        try {
          return secp256k1_context_randomize(byteBuff, Secp256k1Context.getContext()) == 1;
        } finally {
          w.unlock();
        }
    }

    /**
     * This helper method is needed to resolve issue 1524 on bitcoin-s
     * This is because the API changed for ByteBuffer between jdks < 9 and jdk >= 9
     * In the earlier versions of the jdk, a [[java.nio.Buffer]] is returned, but greather than jdk 8
     * returns a [[ByteBuffer]]. This causes issues when compiling with jdk 11 but running with jdk 8
     * as the APIs are incompatible.
     * @see https://github.com/bitcoin-s/bitcoin-s/issues/1524
     * @param byteBuff
     */
    private static void safeRewind(ByteBuffer byteBuff) {
        ((Buffer) byteBuff).rewind();
    }

    private static native long secp256k1_ctx_clone(long context);

    private static native int secp256k1_context_randomize(ByteBuffer byteBuff, long context);

    private static native byte[][] secp256k1_privkey_tweak_add(ByteBuffer byteBuff, long context);

    private static native byte[][] secp256k1_privkey_tweak_mul(ByteBuffer byteBuff, long context);

    private static native byte[][] secp256k1_pubkey_tweak_add(ByteBuffer byteBuff, long context, int pubLen, boolean compressed);

    private static native byte[][] secp256k1_pubkey_tweak_mul(ByteBuffer byteBuff, long context, int pubLen, boolean compressed);

    private static native void secp256k1_destroy_context(long context);

    private static native int secp256k1_ecdsa_verify(ByteBuffer byteBuff, long context, int sigLen, int pubLen);

    private static native byte[][] secp256k1_ecdsa_sign(ByteBuffer byteBuff, long context);

    private static native byte[][] secp256k1_ecdsa_sign_with_entropy(ByteBuffer byteBuff, long context);

    private static native int secp256k1_ec_seckey_verify(ByteBuffer byteBuff, long context);

    private static native byte[][] secp256k1_ec_pubkey_create(ByteBuffer byteBuff, long context, boolean compressed);

    private static native byte[][] secp256k1_ec_pubkey_combine(ByteBuffer byteBuff, long context, int pubLen, int numKeys, boolean compressed);

    private static native byte[][] secp256k1_ec_pubkey_decompress(ByteBuffer byteBuff, long context, int inputLen);

    private static native byte[][] secp256k1_ecdh(ByteBuffer byteBuff, long context, int inputLen);

    private static native byte[][] secp256k1_schnorrsig_sign(ByteBuffer byteBuff, long context);

    private static native byte[][] secp256k1_schnorrsig_sign_with_nonce(ByteBuffer byteBuff, long context);

    //private static native byte[][] secp256k1_schnorrsig_compute_sigpoint(ByteBuffer byteBuff, long context, boolean compressed);

    private static native int secp256k1_schnorrsig_verify(ByteBuffer byteBuffer, long context);

    private static native byte[][] secp256k1_ecdsa_adaptor_sign(ByteBuffer byteBuff, long context, int adaptorLen);

    private static native int secp256k1_ecdsa_adaptor_sig_verify(ByteBuffer byteBuff, long context, int pubKeyLen);

    private static native byte[][] secp256k1_ecdsa_adaptor_adapt(ByteBuffer byteBuff, long context);

    private static native byte[][] secp256k1_ecdsa_adaptor_extract_secret(ByteBuffer byteBuff, long context, int sigLen, int adaptorLen);
}
