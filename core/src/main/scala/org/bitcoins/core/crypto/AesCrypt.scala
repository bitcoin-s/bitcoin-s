package org.bitcoins.core.crypto

import java.security.SecureRandom

import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import javax.crypto.{BadPaddingException, Cipher, SecretKey}
import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}

case class AesEncryptedData(
    cipherText: ByteVector,
    iv: ByteVector,
    salt: AesSalt)

case class AesSalt(
    value: ByteVector
)

/**
  * @throws IllegalArgumentException if passed an empty string
  */
case class AesPassword(value: String) {
  require(value.nonEmpty, "AES passwords cannot be empty!")
}

/**
  * Provides functionality for encrypting and decrypting with AES
  *
  * @note Most of the content here is taken from
  *       [[https://stackoverflow.com/questions/992019/java-256-bit-aes-password-based-encryption this SO question]]
  */
object AesCrypt {

  /**
    * AES encryption with block cipher mode and PBKDF2
    *
    * @see [[https://stackoverflow.com/questions/1220751/how-to-choose-an-aes-encryption-mode-cbc-ecb-ctr-ocb-cfb SO question]]
    *     on different cipher modes with AES
    *
    * @see [[https://www.ietf.org/rfc/rfc2898.txt IETF RFC2898: PKCS #5: Password-Based Cryptography Specification]]
    *     on PBKDF2
    */
  private val aesCipherType: String = "AES/CBC/PKCS5Padding"

  private def getCipher: Cipher = {
    val cipher = Cipher.getInstance(aesCipherType)
    cipher
  }

  private def decryptionCipher(
      secret: SecretKey,
      initializationVector: ByteVector): Cipher = {
    val cipher = getCipher
    cipher.init(Cipher.DECRYPT_MODE,
                secret,
                new IvParameterSpec(initializationVector.toArray))
    cipher
  }

  /**
    * Decrypts the provided data
    */
  def decrypt(
      encrypted: AesEncryptedData,
      password: AesPassword): Either[AesDecryptionException, ByteVector] = {
    if (password.value.isEmpty) {
      Left(AesException.EmptyPasswordException)
    } else {
      val secret = getSecretKey(password, encrypted.salt)
      val cipher = decryptionCipher(secret, encrypted.iv)

      val decryptionAttempt = Try {
        val plainText = cipher.doFinal(encrypted.cipherText.toArray)
        ByteVector(plainText)
      }

      decryptionAttempt match {
        case Success(bytes) => Right(bytes)
        // here we assume that bad padding is because of a bad password
        // provided. assuming that our implementation is correct, correct
        // passwords should never to lead to bad padding
        case Failure(_: BadPaddingException) =>
          Left(AesException.BadPasswordException)
        case Failure(exception) => throw exception
      }
    }
  }

  private def encryptionCipher(secret: SecretKey): Cipher = {
    val cipher = getCipher
    cipher.init(Cipher.ENCRYPT_MODE, secret)
    cipher
  }

  private def getSecureRandom: SecureRandom = new SecureRandom

  /**
    * Generates a random salt with underlying hex string of length 32
    */
  private def getSalt: AesSalt = {
    val rand = getSecureRandom
    val array = new Array[Byte](32)

    rand.nextBytes(array)
    require(array.exists(_ != 0), s"Array for salt must be initialized")

    AesSalt(ByteVector(array))
  }

  /**
    * Encrypts the given plaintext with the given password.
    */
  def encrypt(plainText: ByteVector, password: AesPassword): Either[
    AesEncryptionException,
    AesEncryptedData] = {
    val salt = getSalt
    if (password.value.isEmpty) {
      Left(AesException.EmptyPasswordException)
    } else {
      val secretKey = getSecretKey(password, salt)
      val cipher = encryptionCipher(secretKey)
      val params = cipher.getParameters
      val iv = params.getParameterSpec(classOf[IvParameterSpec]).getIV
      val cipherText = cipher.doFinal(plainText.toArray)

      val encrypted =
        AesEncryptedData(cipherText = ByteVector(cipherText),
                         iv = ByteVector(iv),
                         salt = salt)
      Right(encrypted)
    }

  }

  /**
    * Encrypts the given plaintext with the given password. Throws if invalid
    * data is provided.
    *
    * @throws org.bitcoins.core.crypto.AesEncryptionException If the encryption fails
    */
  def encryptExc(
      plainText: ByteVector,
      password: AesPassword): AesEncryptedData = {
    encrypt(plainText, password) match {
      case Right(value) => value
      case Left(exc)    => throw exc
    }
  }

  private val KEY_SIZE = 256
  private val ITERATIONS = 65536

  /**
    * Generates a secret key used for AES encryption and decryption
    * given the provided password and salt.
    */
  def getSecretKey(password: AesPassword, salt: AesSalt): SecretKey = {
    val tmp: SecretKey = PBKDF2.withSha512(string = password.value,
                                           salt = salt.value.toHex,
                                           iterationCount = ITERATIONS,
                                           derivedKeyLength = KEY_SIZE)
    new SecretKeySpec(tmp.getEncoded, "AES")
  }
}

sealed trait AesEncryptionException extends Exception

sealed trait AesDecryptionException extends Exception

object AesException {
  case object BadPasswordException
      extends BadPaddingException("Bad password provided for decryption")
      with AesDecryptionException

  case object EmptyPasswordException
      extends Exception("Cannot provide an empty password!")
      with AesEncryptionException
      with AesDecryptionException
}
