package org.bitcoin;

import com.google.common.io.BaseEncoding;
import java.util.Arrays;
import java.math.BigInteger;
import javax.xml.bind.DatatypeConverter;
import static org.bitcoin.NativeSecp256k1Util.*;

/**
 * This class holds test cases defined for testing this library.
 */
public class NativeSecp256k1Test {

    private static final BaseEncoding hexEncoder = BaseEncoding.base16();
    //TODO improve comments/add more tests
    /**
      * This tests verify() for a valid signature
      */
    public static void testVerifyPos() throws AssertFailException{
        boolean result = false;
        byte[] data = hexEncoder.decode("CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A90"); //sha256hash of "testing"
        byte[] sig = hexEncoder.decode("3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589");
        byte[] pub = hexEncoder.decode("040A629506E1B65CD9D2E0BA9C75DF9C4FED0DB16DC9625ED14397F0AFC836FAE595DC53F8B0EFE61E703075BD9B143BAC75EC0E19F82A2208CAEB32BE53414C40");

        result = NativeSecp256k1.verify( data, sig, pub);
        assertEquals( result, true , "testVerifyPos");
    }

    /**
      * This tests verify() for a non-valid signature
      */
    public static void testVerifyNeg() throws AssertFailException{
        boolean result = false;
        byte[] data = hexEncoder.decode("CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A91"); //sha256hash of "testing"
        byte[] sig = hexEncoder.decode("3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589");
        byte[] pub = hexEncoder.decode("040A629506E1B65CD9D2E0BA9C75DF9C4FED0DB16DC9625ED14397F0AFC836FAE595DC53F8B0EFE61E703075BD9B143BAC75EC0E19F82A2208CAEB32BE53414C40");

        result = NativeSecp256k1.verify( data, sig, pub);
        //System.out.println(" TEST " + new BigInteger(1, resultbytes).toString(16));
        assertEquals( result, false , "testVerifyNeg");
    }

    /**
      * This tests secret key verify() for a valid secretkey
      */
    public static void testSecKeyVerifyPos() throws AssertFailException{
        boolean result = false;
        byte[] sec = hexEncoder.decode("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530");

        result = NativeSecp256k1.secKeyVerify( sec );
        //System.out.println(" TEST " + new BigInteger(1, resultbytes).toString(16));
        assertEquals( result, true , "testSecKeyVerifyPos");
    }

    /**
      * This tests secret key verify() for a invalid secretkey
      */
    public static void testSecKeyVerifyNeg() throws AssertFailException{
        boolean result = false;
        byte[] sec = hexEncoder.decode("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");

        result = NativeSecp256k1.secKeyVerify( sec );
        //System.out.println(" TEST " + new BigInteger(1, resultbytes).toString(16));
        assertEquals( result, false , "testSecKeyVerifyNeg");
    }

    /**
     * Tests that we can parse public keys
     * @throws AssertFailException
     */
    public static void testIsValidPubKeyPos() throws AssertFailException {
        byte[] pubkey = hexEncoder.lowerCase().decode("0456b3817434935db42afda0165de529b938cf67c7510168a51b9297b1ca7e4d91ea59c64516373dd2fe6acc79bb762718bc2659fa68d343bdb12d5ef7b9ed002b");
        byte[] compressedPubKey = hexEncoder.lowerCase().decode("03de961a47a519c5c0fc8e744d1f657f9ea6b9a921d2a3bceb8743e1885f752676");

        boolean result1 = NativeSecp256k1.isValidPubKey(pubkey);
        boolean result2 = NativeSecp256k1.isValidPubKey(compressedPubKey);

        assertEquals(result1, true, "Uncompressed pubkey parsed failed");
        assertEquals(result2, true, "Compressed pubkey parsed failed");
    }
    public static void testIsValidPubKeyNeg() throws AssertFailException {
        //do we have test vectors some where to test this more thoroughly?
        byte[] pubkey = hexEncoder.decode("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
        boolean result1 = NativeSecp256k1.isValidPubKey(pubkey);

        assertEquals(result1, false, "Compressed pubkey parsed succeeded when it should have failed");
    }
    /**
      * This tests public key create() for a valid secretkey
      */
    public static void testPubKeyCreatePos() throws AssertFailException{
        byte[] sec = hexEncoder.decode("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530");

        byte[] resultArr = NativeSecp256k1.computePubkey( sec,false);
        String pubkeyString = javax.xml.bind.DatatypeConverter.printHexBinary(resultArr);
        assertEquals( pubkeyString , "04C591A8FF19AC9C4E4E5793673B83123437E975285E7B442F4EE2654DFFCA5E2D2103ED494718C697AC9AEBCFD19612E224DB46661011863ED2FC54E71861E2A6" , "testPubKeyCreatePos");
    }

    /**
      * This tests public key create() for a invalid secretkey
      */
    public static void testPubKeyCreateNeg() throws AssertFailException{
       byte[] sec = hexEncoder.decode("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");

       byte[] resultArr = NativeSecp256k1.computePubkey( sec,false);
       String pubkeyString = javax.xml.bind.DatatypeConverter.printHexBinary(resultArr);
       assertEquals( pubkeyString, "" , "testPubKeyCreateNeg");
    }

    /**
      * This tests sign() for a valid secretkey
      */
    public static void testSignPos() throws AssertFailException{

        byte[] data = hexEncoder.decode("CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A90"); //sha256hash of "testing"
        byte[] sec = hexEncoder.decode("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530");

        byte[] resultArr = NativeSecp256k1.sign(data, sec);
        String sigString = javax.xml.bind.DatatypeConverter.printHexBinary(resultArr);
        assertEquals( sigString, "30440220182A108E1448DC8F1FB467D06A0F3BB8EA0533584CB954EF8DA112F1D60E39A202201C66F36DA211C087F3AF88B50EDF4F9BDAA6CF5FD6817E74DCA34DB12390C6E9" , "testSignPos");
    }

    /**
      * This tests sign() for a invalid secretkey
      */
    public static void testSignNeg() throws AssertFailException{
        byte[] data = hexEncoder.decode("CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A90"); //sha256hash of "testing"
        byte[] sec = hexEncoder.decode("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");

        byte[] resultArr = NativeSecp256k1.sign(data, sec);
        String sigString = javax.xml.bind.DatatypeConverter.printHexBinary(resultArr);
        assertEquals( sigString, "" , "testSignNeg");
    }

    /**
      * This tests private key tweak-add
      */
    public static void testPrivKeyTweakAdd_1() throws AssertFailException {
        byte[] sec = hexEncoder.decode("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530");
        byte[] data = hexEncoder.decode("3982F19BEF1615BCCFBB05E321C10E1D4CBA3DF0E841C2E41EEB6016347653C3"); //sha256hash of "tweak"

        byte[] resultArr = NativeSecp256k1.privKeyTweakAdd( sec , data );
        String sigString = javax.xml.bind.DatatypeConverter.printHexBinary(resultArr);
        assertEquals( sigString , "A168571E189E6F9A7E2D657A4B53AE99B909F7E712D1C23CED28093CD57C88F3" , "testPrivKeyAdd_1");
    }

    /**
      * This tests private key tweak-mul
      */
    public static void testPrivKeyTweakMul_1() throws AssertFailException {
        byte[] sec = hexEncoder.decode("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530");
        byte[] data = hexEncoder.decode("3982F19BEF1615BCCFBB05E321C10E1D4CBA3DF0E841C2E41EEB6016347653C3"); //sha256hash of "tweak"

        byte[] resultArr = NativeSecp256k1.privKeyTweakMul( sec , data );
        String sigString = javax.xml.bind.DatatypeConverter.printHexBinary(resultArr);
        assertEquals( sigString , "97F8184235F101550F3C71C927507651BD3F1CDB4A5A33B8986ACF0DEE20FFFC" , "testPrivKeyMul_1");
    }

    /**
      * This tests private key tweak-add uncompressed
      */
    public static void testPrivKeyTweakAdd_2() throws AssertFailException {
        byte[] pub = hexEncoder.decode("040A629506E1B65CD9D2E0BA9C75DF9C4FED0DB16DC9625ED14397F0AFC836FAE595DC53F8B0EFE61E703075BD9B143BAC75EC0E19F82A2208CAEB32BE53414C40");
        byte[] data = hexEncoder.decode("3982F19BEF1615BCCFBB05E321C10E1D4CBA3DF0E841C2E41EEB6016347653C3"); //sha256hash of "tweak"

        byte[] resultArr = NativeSecp256k1.pubKeyTweakAdd( pub , data, false );
        String sigString = javax.xml.bind.DatatypeConverter.printHexBinary(resultArr);
        assertEquals( sigString , "0411C6790F4B663CCE607BAAE08C43557EDC1A4D11D88DFCB3D841D0C6A941AF525A268E2A863C148555C48FB5FBA368E88718A46E205FABC3DBA2CCFFAB0796EF" , "testPrivKeyAdd_2");
    }

    /**
      * This tests private key tweak-mul uncompressed
      */
    public static void testPrivKeyTweakMul_2() throws AssertFailException {
        byte[] pub = hexEncoder.decode("040A629506E1B65CD9D2E0BA9C75DF9C4FED0DB16DC9625ED14397F0AFC836FAE595DC53F8B0EFE61E703075BD9B143BAC75EC0E19F82A2208CAEB32BE53414C40");
        byte[] data = hexEncoder.decode("3982F19BEF1615BCCFBB05E321C10E1D4CBA3DF0E841C2E41EEB6016347653C3"); //sha256hash of "tweak"

        byte[] resultArr = NativeSecp256k1.pubKeyTweakMul( pub , data, false );
        String sigString = javax.xml.bind.DatatypeConverter.printHexBinary(resultArr);
        assertEquals( sigString , "04E0FE6FE55EBCA626B98A807F6CAF654139E14E5E3698F01A9A658E21DC1D2791EC060D4F412A794D5370F672BC94B722640B5F76914151CFCA6E712CA48CC589" , "testPrivKeyMul_2");
    }

    /**
      * This tests seed randomization
      */
    public static void testRandomize() throws AssertFailException {
        byte[] seed = hexEncoder.decode("A441B15FE9A3CF56661190A0B93B9DEC7D04127288CC87250967CF3B52894D11"); //sha256hash of "random"
        boolean result = NativeSecp256k1.randomize(seed);
        assertEquals( result, true, "testRandomize");
    }

    public static void testCreateECDHSecret() throws AssertFailException{

        byte[] sec = hexEncoder.decode("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530");
        byte[] pub = hexEncoder.decode("040A629506E1B65CD9D2E0BA9C75DF9C4FED0DB16DC9625ED14397F0AFC836FAE595DC53F8B0EFE61E703075BD9B143BAC75EC0E19F82A2208CAEB32BE53414C40");

        byte[] resultArr = NativeSecp256k1.createECDHSecret(sec, pub);
        String ecdhString = javax.xml.bind.DatatypeConverter.printHexBinary(resultArr);
        assertEquals( ecdhString, "2A2A67007A926E6594AF3EB564FC74005B37A9C8AEF2033C4552051B5C87F043" , "testCreateECDHSecret");
    }

    public static void main(String[] args) throws AssertFailException{


        System.out.println("\n libsecp256k1 enabled: " + Secp256k1Context.isEnabled() + "\n");

        assertEquals( Secp256k1Context.isEnabled(), true, "isEnabled" );

        //Test verify() success/fail
        testVerifyPos();
        testVerifyNeg();

        //Test secKeyVerify() success/fail
        testSecKeyVerifyPos();
        testSecKeyVerifyNeg();

        //Test parsing public keys
        testIsValidPubKeyPos();
        testIsValidPubKeyNeg();

        //Test computePubkey() success/fail
        testPubKeyCreatePos();
        testPubKeyCreateNeg();

        //Test sign() success/fail
        testSignPos();
        testSignNeg();

        //Test privKeyTweakAdd() 1
        testPrivKeyTweakAdd_1();

        //Test privKeyTweakMul() 2
        testPrivKeyTweakMul_1();

        //Test privKeyTweakAdd() 3
        testPrivKeyTweakAdd_2();

        //Test privKeyTweakMul() 4
        testPrivKeyTweakMul_2();

        //Test randomize()
        testRandomize();

        //Test ECDH
        testCreateECDHSecret();

        NativeSecp256k1.cleanup();

        System.out.println(" All tests passed." );

    }
}
