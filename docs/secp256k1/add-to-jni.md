---
id: jni-modify
title: Adding to Secp256k1 JNI
---

Bitcoin-S uses a Java Native Interface (JNI) to execute functions in [secp256k1](https://github.com/bitcoin-core/secp256k1) from java/scala. The native java bindings used to be a part of the secp256k1 library that was maintained by bitcoin-core, but it was [removed in October 2019](https://github.com/bitcoin-core/secp256k1/pull/682). We maintain a [fork of secp256k1](https://github.com/bitcoin-s/secp256k1) which forks off of bitcoin-core's `master` but re-introduces the jni. This is also the easiest way to add functionality from new projects such as [Schnorr signatures](https://github.com/bitcoin-core/secp256k1/pull/558) and [ECDSA adaptor signatures](https://github.com/jonasnick/secp256k1/pull/14) by rebasing the bitcoin-s branch with the JNI on top of these experimental branches. That said, it is quite tricky to hook up new functionality in secp256k1 into bitcoin-s and specifically `NativeSecp256k1.java`. The following is a description of this process.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
<!-- END doctoc -->

- [Adding a new function to NativeSecp256k1.java](#adding-a-new-function-to-nativesecp256k1java)
  - [Adding to `src/java/org_bitcoin_NativeSecp256k1.c`](#adding-to-srcjavaorg_bitcoin_nativesecp256k1c)
  - [Adding to `src/java/org_bitcoin_NativeSecp256k1.h`](#adding-to-srcjavaorg_bitcoin_nativesecp256k1h)
  - [Adding to `src/java/org/bitcoin/NativeSecp256k1.java`](#adding-to-srcjavaorgbitcoinnativesecp256k1java)
  - [Adding to `src/java/org/bitcoin/NativeSecp256k1Test.java`](#adding-to-srcjavaorgbitcoinnativesecp256k1testjava)
  - [Adding to Bitcoin-S](#adding-to-bitcoin-s)
  - [Further Work to Enable Typed Invocations and Nice Tests](#further-work-to-enable-typed-invocations-and-nice-tests)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Adding a new function to NativeSecp256k1.java

 ### Adding to `src/java/org_bitcoin_NativeSecp256k1.c`

1. Add an `#include` import at the top (if applicable)

   If your secp256k1 functions are not already included, you will need to `#include` the header file (should be in the `secp256k1/include` directory).

2. Function signature

   Your new function signature should begin with

   ```c
   SECP256K1_API jint JNICALL Java_org_bitcoin_NativeSecp256k1_
   ```

   followed by the secp256k1 function name where `_` are replaced with `_1` (it's a weird jni thing). Finally, you add a parameter list that begins with

   ```c
   (JNIEnv* env, jclass classObject, jobject byteBufferObject, jlong ctx_l
   ```

   and ends with any `jint`s in the case that any of the secp256k1 function inputs have variable length (such as public keys which can be either `33` or `65` bytes, or ECDSA signatures), and lastly any `jboolean`s in case there is some flag like `compressed` passed in.

   As an example that includes everything, if you are making a call to `secp256k1_pubkey_tweak_add` which takes in public keys that could be `33` or `65` bytes and outputs a public key that will either be compressed or decompressed based on an input flag, then the function signature would be

   ```c
   SECP256K1_API jobjectArray JNICALL Java_org_bitcoin_NativeSecp256k1_secp256k1_1pubkey_1tweak_1add
     (JNIEnv* env, jclass classObject, jobject byteBufferObject, jlong ctx_l, jint publen, jboolean compressed)
   ```

3. Reading `unsigned char*` inputs

   It is now time to create pointers for each of the secp256k1 function inputs that where passed in via the `byteBufferObject`. We must first read in the `Secp256k1Context` with the line

   ```c
   secp256k1_context *ctx = (secp256k1_context*)(uintptr_t)ctx_l;
   ```

   and we can then initialize the first pointer to be the beginning of the `byteBufferObject` with the line

   ```c
   unsigned char* firstArg = (*env)->GetDirectBufferAddress(env, byteBufferObject);
   ```

   and subsequent arguments' pointers where the previous argument's length is known (say `32` bytes for example) can be instantiated using

   ```c
   unsigned char* prevArg = ...
   unsigned char* nextArg = (unsigned char*) (prevArg + 32);
   ```

   and in the case that a previous argument has variable length, then a `jint` has been provided as an input and can be used instead, such as in the example

   ```c
   unsigned char* prevArg = ...
   unsigned char* nextArg = (unsigned char*) (prevArg + publen);
   ```

   where `publen` is a `jint` passed to this C function.

   As an example that includes everything, consider the function `secp256k1_ecdsa_verify` which takes as input a `32` byte message, a variable length signature and a public key (of length `33` or `65` bytes). Our function will begin with

   ```c
   {
     secp256k1_context *ctx = (secp256k1_context*)(uintptr_t)ctx_l;
   
     unsigned char* data = (unsigned char*) (*env)->GetDirectBufferAddress(env, byteBufferObject);
     const unsigned char* sigdata = (unsigned char*) (data + 32);
     const unsigned char* pubdata = (unsigned char*) (sigdata + siglen);
   ```

   where `siglen` is a `jint` passed into the C function.

4. Initialize variables

   Next we must declare all variables. We put all decelerations here as it is required by the C framework used by `libsecp256k1` that definitions and assignments/function calls cannot be interleaved.

   Specifically you will need to declare any secp256k1 specific structs here as well as all outputs (such as `jobjectArrays` and `jByteArrays`). Generally speaking this will include all inputs which are not raw data (public keys, signatures, etc). Lastly, you will also have an `int ret` which will store `0` if an error occurred and `1` otherwise.

   As an example that includes everything, consider again the function `secp256k1_pubkey_tweak_add` has the following declarations

   ```c
   jobjectArray retArray;
   jbyteArray pubArray, intsByteArray;
   unsigned char intsarray[2];
   unsigned char outputSer[65];
   size_t outputLen = 65;
   secp256k1_pubkey pubkey;
   int ret;
   ```

   Where `retArray` is eventually going to be the data returned, which will contain the `jbyteArray`s `pubArray` and `intsByteArray`, which will contain `outputSer` and `intsarray` respectively. Lastly `pubkey` will store a deserialized `secp256k1_pubkey` corresponding to the input `unsigned char*` public key.

5. Parse inputs when applicable

   In the case where there are `unsigned char*` inputs which need to be deserialized into secp256k1 structs, this is done now. As an example, `secp256k1_pubkey_tweak_add` takes a public key as input:

   ```c
   unsigned char* pkey = (*env)->GetDirectBufferAddress(env, byteBufferObject);
   ```

   where a `jint publen` is passed in as a function parameter. This function already has a declaration for `secp256k1_pubkey pubkey;`. The first call made after the above declarations is

   ```c
   ret = secp256k1_ec_pubkey_parse(ctx, &pubkey, pkey, publen)
   ```

   and if further parsing is necessary, it is put inside of `if (ret) { ret = [further parsing here] }`.

6. Make calls to secp256k1 functions to instantiate outputs

   It is finally time to actually call the secp256k1 function we are binding to the jni! This is done by simply calling `ret = [call to secp function here];` or `if (ret) { ret = [secp function call] };` if there were any inputs that needed to be parsed. Note that some secp256k1 functions return outputs by populating variables you should have declared and for which pointers are passed as inputs, while other functions will mutate their inputs rather than returning outputs.

7. Serialize variable length outputs if applicable

   When dealing with variable length outputs such as signatures, you will likely need to serialize these outputs. This is done by having already instantiated such a variable as

   ```c
   unsigned char outputSer[72];
   size_t outputLen = 72;
   ```

   where in this case `72` is an upper bound on signature length. With these variables existing (as well as a `secp256k1_ecdsa_signature sig` which has been populated), we call a secp256k1 serialization function to populate `outputSer` and `outputLen` from `sig`  

   ```c
   if(ret) {
       int ret2 = secp256k1_ecdsa_signature_serialize_der(ctx, outputSer, &outputLen, &sig);
       (void)ret2;
     }
   ```

   As you can see, in this case we do not which to alter the value returned in `ret` if serialization fails. If we did then `ret2` would not be introduced and we would instead do `ret = [serialize]`.

8. Populate return array when applicable

   We now begin translating our serialized results back into Java entities. If you are returning any `int`s containing meta-data (usually `ret` is included here, as are the variable lengths of outputs when applicable), you will want an `unsigned char intsarray[n]` to be already declared where `n`  is the number of pieces of meta-data. For example, in `secp256k1_ecdsa_sign`, we wish to return whether there were any errors (stored in `int ret`) and the output signature's length, `size_t outputLen`. Hence we have an `unsigned char intsarray[2]` and we populate it as follows

   ```c
   intsarray[0] = outputLen;
   intsarray[1] = ret;
   ```

    Next we populate the `jobjectArray` we wish to return, this will always begin with a call

   ```c
   retArray = (*env)->NewObjectArray(env, 2,
     (*env)->FindClass(env, "[B"),
     (*env)->NewByteArray(env, 1));
   ```

   to instantiate an empty return array. Next we instantiate our `jbyteArray`s with calls to

   ```c
   myByteArray = (*env)->NewByteArray(env, len);
   ```

   where `myByteArray` is replaced with a real name (such as `intsByteArray`) and `len` is replaced with the length of this array (either a constant or a populated `size_t` variable). Next we populate this array with our data by calling

   ```c
   (*env)->SetByteArrayRegion(env, myByteArray, 0, len, (jbyte*)myData);
   ```

   where `myData` is a C array of `unsigned char` (of length `len`). Lastly, we place `myByteArray` into its place in `retArray` with

   ```c
   (*env)->SetObjectArrayElement(env, retArray, index, myByteArray);
   ```

   where `index` is a constant (`0`, `1`, `2`, etc.) for the index of `myByteArray` within `retArray`. Note that you should follow our conventions and have index `0` contain the actual data to be returned and index `1` (and onward) contain any meta-data.

   Please note that if you wish not to return such meta-data (such as if you wish to return only a `boolean`), then none of the code in this subsection is required

9. void `classObject`

   Once we are ready to return, we first void the input `classObject` by making the call

   ```c
   (void)classObject;
   ```

10. Return array when applicable, `ret` when applicable

    Lastly, we return `retArray` in the case where we wish to return a `byte[]`, or `ret` in the case that we wish to return a `boolean`.

### Adding to `src/java/org_bitcoin_NativeSecp256k1.h`

Once your function is defined in `src/java/org_bitcoin_NativeSecp256k1.c`, you must define them in the corresponding header files by simply copying the function signature but without parameter names. For example, if `secp256k1_pubkey_tweak_add` has the function signature

```c
SECP256K1_API jobjectArray JNICALL Java_org_bitcoin_NativeSecp256k1_secp256k1_1pubkey_1tweak_1add
  (JNIEnv* env, jclass classObject, jobject byteBufferObject, jlong ctx_l, jint publen, jboolean compressed)
```

then in the header file we include

```c
/*
 * Class:     org_bitcoin_NativeSecp256k1
 * Method:    secp256k1_pubkey_tweak_add
 * Signature: (Ljava/nio/ByteBuffer;JI)[[B
 */
SECP256K1_API jobjectArray JNICALL Java_org_bitcoin_NativeSecp256k1_secp256k1_1pubkey_1tweak_1add
  (JNIEnv *, jclass, jobject, jlong, jint, jboolean);
```

### Adding to `src/java/org/bitcoin/NativeSecp256k1.java`

We are now done writing C code! We have completed an interface in C for the JNI to hook up to. However, we must now write the corresponding Java code which hides the Java to C (and back) conversions from other Java code. We accomplish this with a `class` of `static` methods called `NativeSecp256k1`.

1. Add `private static native` secp256k1 function

   We begin by adding a `private static native` method at the bottom of the file corresponding to our secp256k1 function. Notice that the syntax for `native` methods is similar to that of Java abstract interface methods where instead of providing an implementation we simply end with a semi-colon.

   For functions returning `boolean`s, we have their `native` methods return `int` (will be `0` or `1`). Otherwise, for functions returning `byte[]`s, we have their `native` methods return `byte[][]` (two dimensional array to allow for meta-data).

2. Method signature

   Next we add a method to the `NativeSecp256k1` class

   ```java
   public static byte[] myFunc(byte[] input1, byte[] input2, boolean input3) throws AssertFailException
   ```

   where `boolean` could also be the return type instead of `byte[]`.

3. `checkArgument`s

   We begin implementing this function by checking the input argument lengths using the `checkArument` function

   ```java
   checkArgument(input1.length == 32 && (input2.length == 33 || input2.length == 65));
   ```

4. Initialize `ByteBuffer`

   We now initialize the `ByteBuffer` which we will be passing through the JNI as an input. This is done with a call to

   ```java
   ByteBuffer byteBuff = nativeECDSABuffer.get();
   ```

   followed by allocation when necessary as follows

   ```java
   if (byteBuff == null || byteBuff.capacity() < input1.length + input2.length) {
       byteBuff = ByteBuffer.allocateDirect(input1.length + input2.length);
       byteBuff.order(ByteOrder.nativeOrder());
       nativeECDSABuffer.set(byteBuff);
   }
   ```

   where `input1.length + input2.length` is replaced by whatever the total `ByteBuffer` length needed.

5. Fill `ByteBuffer`

   We now populate the `ByteBuffer` as follows

   ```java
   byteBuff.rewind();
   byteBuff.put(input1);
   byteBuff.put(input2);
   ```

   where generally, you will `rewind()` and then `put()` all inputs (in order).

6. Make `native` call

   It is now time to make a call to our `native` C function.

   In the case where we are returning a `byte[]`, this is done by first declaring a `byte[][]` to store the output and then locking the read lock, `r`. Then we call the `native` function within a `try` clause which releases the lock in the `finally` clause.

   ```java
   byte[][] retByteArray;
   r.lock();
   try {
       retByteArray = secp256k1_my_call(byteBuff, Secp256k1Context.getContext(), input3);
   } finally {
       r.unlock();
   }
   ```

   In the case where we are returning a `boolean`, simply make the call in the `try` and compare the output to `1` like so

   ```java
   r.lock();
   try {
       return secp256k1_my_bool_call(byteBuff, Secp256k1Context.getContext()) == 1;
   } finally {
       r.unlock();
   }
   ```

   If this is the case, you are now done and can ignore the following steps.

7. Parse outputs

   `retByteArray` should now be populated and we want to read its two parts (data and meta-data). Getting the data should be as easy as

   ```java
   byte[] resultArr = retByteArr[0];
   ```

   while for each piece of meta-data, you can read the corresponding `int` as follows

   ```java
   int metaVal = new BigInteger(new byte[] { retByteArray[1][index] }).intValue();
   ```

   where `index` is replaced with the index in the meta-data array.

8. Validate outputs

   In the case where we now have meta-data, we validate it with calls to `assertEquals`.

9. Return output

   Finally, we return `resultArr`.

### Adding to `src/java/org/bitcoin/NativeSecp256k1Test.java`

I normally first build the C binaries and add to Bitcoin-S before coming back to this section because I use `sbt core/console` to generate values and make calls below, but this is not a requirement.

1. Generate values and make calls to `org.bitcoin.NativeSecp256k1` to generate inputs and their expected outputs

2. Create regression unit tests with these values in NativeSecp256k1Test

   Note that you can use `DatatypeConverter.parseHexBinary` to convert `String` hex to a `byte[]`, and you can use `DatatypeConverter.printHexBinary` to convert a `byte[]` to its `String` hex. Lastly you will make assertions with calls to `assertEquals`.

3. Add test to `main`

### Adding to Bitcoin-S

1. Translate `NativeSecp256k1` and `NativeSecp256k1Test` to jni project

   By translate I mean to say that you must copy the functions from those files to the corresponding files in the `bitcoin-s/secp256k1jni` project. For tests this will require changing calls to `DatatypeConverter` to either `toByteArray` or `toHex` as well as changing the method to make it non-`static` as well as adding the  `@Test` annotation above the method (rather than adding to a `main` method).

2. Configure and build `secp256k1`

   You will need to go to the `bitcoin-s/secp256k1` directory in a terminal and running the following where you may need to add to the `./configure` command if you are introducing a new module.

   __For Linux or OSx (64-bit)__

   You will have to make sure `JAVA_HOME` is set, and build tools are installed, for Linux this requires:

   ```bashrc
   echo JAVA_HOME
   sudo apt install build-essential autotools-dev libtool automake
   ```

   and for Mac this requires:

   ```bashrc
   brew install automake libtool
   ```

   You should then be able to build `libsecp256k1` with the following:

   ```bashrc
   ./autogen.sh
   ./configure --enable-jni --enable-experimental --enable-module-ecdh
   make
   make check
   make check-java
   ```

   __For Windows (64-bit)__

   Windows bindings are cross-built on Linux. You need to install the `mingw` toolchain and have `JAVA_HOME` point to a Windows JDK:

   ```bashrc
   sudo apt install g++-mingw-w64-x86-64
   sudo update-alternatives --config x86_64-w64-mingw32-g++
   ```

   You should then be able to build `libsecp256k1` with the following:

   ```bashrc
   echo "LDFLAGS = -no-undefined" >> Makefile.am
   ./configure --host=x86_64-w64-mingw32 --enable-experimental --enable-module_ecdh --enable-jni && make clean && make CFLAGS="-std=c99"
   ```

   There may be some errors that can be ignored:
      - `Could not determine the host path corresponding to`
      - `redeclared without dllimport attribute: previous dllimport ignored`

3. Copy binaries into bitcoin-s natives for your system

   You have now built the C binaries for your JNI bindings for your operating system and you should now find your operating system's directory in `bitcoin-s/secp256k1jni/natives` and replace its contents with the contents of `secp256k1/.libs` (which contains the compiled binaries).

4. Run `secp256k1jni` tests

   If you have not yet implemented tests, you should now be able to go back and do so as calls to `NativeSecp256k1` should now succeed.

   Once you have tests implemented, and assuming you've copied them correctly to the `bitcoin-s/secp256k1jni` project, you should be able to run them using

   ```bashrc
   sbt secp256k1jni/test
   ```

### Further Work to Enable Typed Invocations and Nice Tests

1. Add new `NetworkElement`s where applicable

   In the case where you are dealing in new kinds of data that are not yet defined in Bitcoin-S, you should add these as `case class`es extending the `NetworkElement` trait, and give them companion objects extending `Factory` for easy serialization and deserialization.

   This step is not necessary if you are only dealing in raw data, `ECPrivateKey`s, `ECPublicKey`s, etc.

2. Add new typed functions to relevant data types where applicable

   In the case where your new function should be a static method, find a good `object` (or introduce one) and give it a `def` which takes in typed arguments and outputs typed arguments (using `ByteVector` in all places dealing with raw data rather than using `Array[Byte]`). You will then implement these methods using calls to `NativeSecp256k1` methods and getting the inputs into `Array[Byte]` form by getting their `ByteVector`s (usually through a call to `_.bytes`) and then calling `_.toArray`.

   You will then need to take the data returned and deserialize it.

   In the case where your new function belongs naturally as an action performed by some existing or newly introduced type, you can implement your new function as a call made by that class as described for the previous case but where the class will pass a serialized version of itself into the `NativeSecp256k1` call.

   It is often acceptable to implement the call in an `object` and then also add the call (via a call to the object, passing `this`) to the interface of relevant types. 

3. Implement Bouncy Castle fallback in `BouncyCastleUtil.scala` if you can.

4. Add unit and property-based tests.

5. If you implemented Bouncy Castle fallback, add tests to `BouncyCastleSecp256k1Test` to compare implementations