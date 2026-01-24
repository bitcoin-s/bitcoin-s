package org.bitcoins.crypto.frost

import org.bitcoins.crypto.{ECPublicKey, FieldElement, SecpPointFinite}
import org.bitcoins.crypto.musig.ParityMultiplier

case class FrostSessionValues(
    tweakCtx: FrostTweakContext,
    ids: Vector[Int],
    pubshares: Vector[ECPublicKey],
    b: FieldElement,
    r: SecpPointFinite,
    e: FieldElement) {
  def q: SecpPointFinite = tweakCtx.q

  def gacc: ParityMultiplier = tweakCtx.gacc

  def tacc: FieldElement = tweakCtx.tacc
}
