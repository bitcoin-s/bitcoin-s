package org.bitcoins.node.constant

import org.bitcoins.core.util.EnvUtil

case object NodeConstants {
  lazy val userAgent = s"/bitcoin-s:${EnvUtil.parseVersion(EnvUtil.getVersion)}/"
}
