package org.bitcoins.core.api.tor

case class Credentials(username: String, password: String) {
  require(username.length < 256, "username is too long")
  require(password.length < 256, "password is too long")
}
